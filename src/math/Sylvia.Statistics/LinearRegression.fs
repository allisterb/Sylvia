namespace Sylvia

open System

open FSharp.Quotations.Patterns

open MathNet.Numerics
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearRegression
open MathNet.Numerics.Distributions
open Sylvia.Data

[<AutoOpen>]
module Data =
    let csv_file name = new CsvFile(name)

    let csv_file_with_delim name delim = new CsvFile(name, delim)
    
    let gretl_csv_file name =
        let _name = name + ".csv"
        GretlFile.ToCsvFile(name, _name)
        csv_file _name

    let csv_fields (f:obj) = 
        match f with
        | :? string as fn -> fn |> CsvFile |> Seq.map(fun fn -> sprintf "%A:%A" fn.Label fn.Type) |> Seq.toArray
        | :? CsvFile as csvf -> csvf |> Seq.map(fun fn -> sprintf "%A:%A" fn.Label fn.Type) |> Seq.toArray
        | _ -> failwithf "Cannot get CSV fields from %A" f

    let with_field_type<'t> (col:int) (f:CsvFile) = f.[col].Type <- typeof<'t>; f

    let with_all_field_types<'t> (f:CsvFile) = 
        for i in 0..f.Fields.Count - 1 do f.[i].Type <- typeof<'t>         
        f
    
    let frame (file:CsvFile) = new Frame(file)

    let samples (cols:seq<obj>) (f:CsvFile) =
        let _cols = cols |> Seq.map(
            function 
            | :? string as s -> s 
            | :? realvar as v -> v.Name
            | :? CsvField as cf -> cf.Label
            | _ -> failwithf "Column names must be strings.") |> Seq.toArray 
        let n = Seq.length _cols - 1
        let df = frame f
        let x = _cols |> Seq.take (Seq.length cols - 1) |> Seq.toArray |> df.Sel in
        let y = _cols |> Seq.last 
        Seq.zip (seq {for r in x -> seq {for i in 0 .. n - 1 -> r.[i]}}) (seq {for r in df.[y] -> r})

type LinearRegressionModel(eqn:ScalarVarMap<real>, samples: (real array*real) array, ?var_changes:ScalarVarMap<real> array) =
    let dv = eqn.Var
    let rvn = eqn |> rhs |> get_real_vars |> Seq.map (fun v -> v.Name)
    let terms = 
        match eqn |> rhs |> sexpr |> simplifye with
        | LinearTermsOf rvn t -> t
        | _ -> failwithf "%A is not a linear expression of variables %A." eqn.Rhs rvn
    let b0 = 
        match (terms |> List.tryFind(function| [Constant _] -> true | _ -> false)) with
        | Some [Constant (ValueWithName(_,_,n))] -> ScalarConst<real> n
        | _ -> failwithf "Cannot determine the slope intercept parameter symbol."
    let b1 = 
        terms |> List.filter(function|[Constant c; VariableWithOneOfNames rvn _] -> true | _ -> false) 
        |> List.map (function | [Constant (ValueWithName(_,_,n)); VariableWithOneOfNames rvn v] -> (v |> get_var |> exprvar |> realvar), ScalarConst<real> n | _ -> failwithf "Cannot determine the slope coefficient parameter symbol.")
        |> List.sortBy (snd >> const_name)
    let rv = b1 |> List.map fst 
    let n = samples |> Array.length
    let dof = n - (List.length b1 + 1)
    let xsamples,ysamples = samples |> Array.map fst |> Array.transpose, samples |> Array.map snd
    let xmean,xvar = let mv = xsamples |> Array.map Statistics.MeanVariance in Array.map fst mv, Array.map snd mv
    let ymean,yvar = ysamples |> Statistics.MeanVariance
    let a = 
        if rv.Length = 1 then 
            let p = SimpleRegression.Fit(samples |> Array.map(fun (x,y) -> x.[0], y)) in [|fst p; snd p|] 
        else 
            MultipleRegression.QR(samples, true)
    let re = dv == (a |> Array.skip 1 |> Array.mapi (fun i v -> rv.[i] * v) |> Array.reduce (+)) + a.[0]
    let rf (x:float[]) = (a |> Array.skip 1 |> Array.mapi (fun i v -> x.[i] * v) |> Array.reduce (+)) + (a.[0])  
    let ypred = samples |> Array.map (fst >> rf)
    let rss = samples |> Array.sumBy(fun (x,y) -> (y - rf x) ** 2.)
    let tss = ysamples |> Array.sumBy(fun y -> (y - ymean) ** 2.)
    let ess = tss - rss
    let ser =  sqrt (rss / (real) dof)
    let xtss = xsamples |> Array.mapi(fun i s -> Array.sumBy(fun x -> (x - xmean.[i]) ** 2.) s)
    let sec = 
        let k = rv.Length
        let mX = MathNet.Numerics.LinearAlgebra.Double.DenseMatrix(n, k + 1)
        for r in 0 .. n - 1 do
            mX.[r, 0] <- 1.0
            let x, _ = samples.[r]
            for c in 0 .. k - 1 do
                mX.[r, c + 1] <- x.[c]
        let mXtXInv = (mX.Transpose() * mX).Inverse()
        mXtXInv.Diagonal().Map(fun v -> sqrt(ser * ser * v)).ToArray()
    
    let tstats = a |> Array.mapi (fun i c -> c / sec.[i])
    let tpvalues = tstats |> Array.map (fun t -> 2.0 * (1.0 - Distributions.StudentT.CDF(0.0, 1.0, (float) dof, abs t)))
    let fstat = (ess / (float) rv.Length) / (rss / (float) dof)
    let fpvalue = 1.0 - Distributions.FisherSnedecor.CDF((float) rv.Length, (float) dof, fstat)

    let xr = 
        [|for i in 0 .. xsamples.Length - 1 -> [|for j in 0 .. xsamples.Length - 1 do if i <> j then yield SimpleRegression.FitThroughOrigin(xsamples.[i], xsamples.[j]) |]|]
        
    let oeqn : ScalarEquation<real> option = var_changes |> Option.map(Array.fold(fun e cv -> e.SubstVar(cv.Var, cv.Rhs)) (eqn :> ScalarEquation<real>)) 
    
    member val ModelEquation = eqn
    member val OriginalModelEquation = oeqn 
    member val Samples = samples 
    member val N = n
    member val DependentVariable = eqn.Var
    member val IndependentVariables = rv |> List.toArray
    member val Variables = rv @ [dv] |> List.toArray 
    member val Parameters = [b0] @ (b1 |> List.map snd) |> List.toArray
    member val DegreesOfFreedom = dof
    member val RegressionCoefficients = a
    member val XSamples = xsamples
    member val YSamples = ysamples
    member val XMean = xmean
    member val XVar = xvar
    member val XSd = xvar |> Array.map sqrt
    member val YMean = ymean
    member val YVar = yvar
    member val Ysd = sqrt yvar
    member val Rss = rss
    member val Ess = ess
    member val Tss = tss
    member val Ser = ser
    member val XTss = xtss
    member val Sec = sec
    member val TStats = tstats
    member val TPValues = tpvalues
    member val FStat = fstat
    member val FPValue = fpvalue
    member val RegressionEquation = re
    member val OriginalRegressionEquation : ScalarEquation<real> option = var_changes |> Option.map(fun vc ->  vc |> Array.fold(fun e cv -> e.SubstVar(cv.Var, cv.Rhs)) (re :> ScalarEquation<real>))   
    member val RegressionFunction = rf 
    member val YPredictions = samples |> Array.map (fst >> rf)
    member val R2 : real = GoodnessOfFit.CoefficientOfDetermination(ypred, ysamples)
    member val r : real= GoodnessOfFit.R(ypred, ysamples)

    member __.Item([<ParamArray>] (x:real array)) = rf x
    member __.Copy() = LinearRegressionModel(eqn, samples)
    override x.ToString() = 
        let sb = System.Text.StringBuilder()
        let k = rv.Length
        let adjR2 = 1.0 - (1.0 - x.R2) * (float(n-1))/(float(n-k-1))
        let get_stars p = if p < 0.01 then "***" elif p < 0.05 then "**" elif p < 0.1 then "*" else ""

        sb.AppendLine(sprintf "Model: OLS, using observations 1-%d" n) |> ignore
        sb.AppendLine(sprintf "Dependent variable: %s" dv.Name) |> ignore
        sb.AppendLine(String.replicate 80 "-") |> ignore
        sb.AppendLine(sprintf "%-15s %12s %12s %12s %12s" "" "coefficient" "std. error" "t-ratio" "p-value") |> ignore
        sb.AppendLine(String.replicate 80 "-") |> ignore

        let p_name = x.Parameters.[0].Name
        let p_stars = get_stars x.TPValues.[0]
        sb.AppendLine(sprintf "%-15s %12.6f %12.6f %12.3f %12.4f %s" p_name x.RegressionCoefficients.[0] x.Sec.[0] x.TStats.[0] x.TPValues.[0] p_stars) |> ignore
        
        for i in 0 .. rv.Length - 1 do
            let var_name = x.IndependentVariables.[i].Name
            let stars = get_stars x.TPValues.[i+1]
            sb.AppendLine(sprintf "%-15s %12.6f %12.6f %12.3f %12.4f %s" var_name x.RegressionCoefficients.[i+1] x.Sec.[i+1] x.TStats.[i+1] x.TPValues.[i+1] stars) |> ignore
        
        sb.AppendLine(String.replicate 80 "-") |> ignore
        
        sb.AppendLine(sprintf "%-25s %-15.4f %-25s %-15.4f" "Mean dependent var" x.YMean "S.D. dependent var" x.Ysd) |> ignore
        sb.AppendLine(sprintf "%-25s %-15.4f %-25s %-15.4f" "Sum squared resid" x.Rss "S.E. of regression" x.Ser) |> ignore
        sb.AppendLine(sprintf "%-25s %-15.4f %-25s %-15.4f" "R-squared" x.R2 "Adjusted R-squared" adjR2) |> ignore
        sb.AppendLine(sprintf "%-25s %-15.4f %-25s %-15.4g" (sprintf "F(%d, %d)" k dof) x.FStat "P-value(F)" x.FPValue) |> ignore

        sb.ToString()

    new (eqn:ScalarVarMap<real>, samples: (real array*real) seq) = LinearRegressionModel(eqn, samples |> Seq.toArray)

module LinearRegression =
    let lr (eqn:ScalarVarMap<real>) (data:seq<seq<_>*_>) =
           let d1, d2 = data |> Seq.map (fst>> Seq.map box >> Seq.toArray), data |> Seq.map (snd>>box)
           LinearRegressionModel(eqn, data |> Seq.map(fun(x,y) -> ((Seq.map to_real x) |> Seq.toArray, to_real y))) 
    
    let slr (eqn:ScalarVarMap<real>) (data:seq<_*_>) =
         lr eqn (data |> Seq.map(fun (x, y) -> Seq.singleton x, y))

    let lrvars (m:LinearRegressionModel) = m.Variables

    let lrsamp (m:LinearRegressionModel) = m.Samples
    
    let lrN (m:LinearRegressionModel) = m.N

    let lrdof (m:LinearRegressionModel) = m.DegreesOfFreedom

    let lrdvar (m:LinearRegressionModel) = m.DependentVariable

    let lrivars (m:LinearRegressionModel) = m.IndependentVariables

    let lrparams (m:LinearRegressionModel) = m.Parameters

    let lreqn (m:LinearRegressionModel) = m.RegressionEquation

    let lroeqn (m:LinearRegressionModel) = m.OriginalRegressionEquation

    let lrmeqn (m:LinearRegressionModel) = m.ModelEquation :> ScalarEquation<real> 

    let lromeqn (m:LinearRegressionModel) = m.OriginalModelEquation

    let lrcoeffs (m:LinearRegressionModel) = m.RegressionCoefficients

    let lrxsamp (m:LinearRegressionModel) = m.XSamples

    let lrysamp (m:LinearRegressionModel) = m.YSamples

    let lrxmean (m:LinearRegressionModel) = m.XMean

    let lrymean (m:LinearRegressionModel) = m.YMean

    let lrxvar (m:LinearRegressionModel) = m.XVar

    let lryvar (m:LinearRegressionModel) = m.YVar

    let lrrss (m:LinearRegressionModel) = m.Rss

    let lress (m:LinearRegressionModel) = m.Ess

    let lrtss (m:LinearRegressionModel) = m.Tss

    let lrser (m:LinearRegressionModel) = m.Ser

    let lrxtss (m:LinearRegressionModel) = m.XTss

    let lrsec (m:LinearRegressionModel) = m.Sec

    let lrtstats (m:LinearRegressionModel) = m.TStats

    let lrtpvalues (m:LinearRegressionModel) = m.TPValues

    let lrfstat (m:LinearRegressionModel) = m.FStat

    let lrfpvalue (m:LinearRegressionModel) = m.FPValue

    let lrysd (m:LinearRegressionModel) = m.Ysd

    let lrR2 (m:LinearRegressionModel) = m.R2

    let change_var (eqn:ScalarVarMap<real>) (m:LinearRegressionModel) =
        let rvs = eqn.Rhs |> get_real_vars
        if rvs.Length <> 1 then failwithf "The RHS of the equation: %A is not an expression of a single variable." eqn.Rhs
        let rv = List.exactlyOne rvs
        if not (m.ModelEquation.Var = rv || Array.contains rv m.IndependentVariables) then failwithf "The RHS of the equation does not contain a dependent or independent variable of the regression equation."
        let f = RealFunction eqn
        if m.ModelEquation.Var = rv then
             let samples = m.Samples |> Array.map(fun s -> fst s, s |> snd |> f.Item) in
             LinearRegressionModel(eqn.Var == m.ModelEquation.Rhs, samples, [|eqn|])
        else
            let index = Array.findIndex((=) rv) m.IndependentVariables
            let samples = m.Samples |> Array.map(fun (x,y) -> x |> Array.mapi (fun i _x -> if index = i then f.Item _x else _x), y) in
            let v = eqn.Lhs in
            let p = subst_var_value rv.Var (v.Expr) m.ModelEquation.Rhs.Expr |> expand_as<real> |> simplifye |> Scalar in
            LinearRegressionModel(m.DependentVariable == p, samples, [|eqn|])
        
    let change_vars (eqns:ScalarVarMap<real> seq) (m:LinearRegressionModel) = 
        let cm = eqns |> Seq.fold (fun s e -> change_var e s) m in 
        LinearRegressionModel(cm.ModelEquation, cm.Samples, eqns |> Seq.toArray)

    let new_vars (eqns:ScalarVarMap<real> seq) (m:LinearRegressionModel) = 
           let cm = eqns |> Seq.fold (fun s e -> change_var e s) m in 
           LinearRegressionModel(cm.ModelEquation, cm.Samples)