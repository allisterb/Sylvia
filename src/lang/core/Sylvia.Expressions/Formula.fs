namespace Sylvia

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

// Make Formula an alias for the reflected definition attribute.
type Formula = ReflectedDefinitionAttribute

module Formula =        
    (* Logical operators for formulas *)
    let (===>) l r = not l || r
    let (<===) l r = r ===> l
 
    /// Represents a symbolic formula.
    let formula<'t> = Unchecked.defaultof<'t>

    /// Result of symbolic truth-functional operation.
    let truth_value = formula<bool>
        
    /// A predicate expression
    let pred_expr<'t> n = 
        let var = Expr.Var(Var(n, typeof<'t -> bool>)) in <@ %%var:'t->bool @>

    (* Quantifiers *)

    /// Generic quantifier for binary op that is symmetric, associative and has an identity.
    let quantifier<'t,'u> (op:'t -> 't -> 't) (bound:'u) (range:bool) (body:'t) = formula<'t>

    [<Symbol "\u2200">]
    let forall_expr<'u> (bound:'u) (range:bool) (body:bool) = truth_value
    [<ReflectedDefinition>]
    let forall_expr2<'u> (bound:'u) (body:bool) = forall_expr bound true body
    [<Symbol "\u2203">]
    let exists_expr<'u> (bound:'u) (range:bool) (body:bool) = truth_value
    [<ReflectedDefinition>]
    let exists'<'u> (bound:'u) (body:bool) = exists_expr bound true body

    /// Generic quantifier with sum semantics.
    let sum<'t,'u> (op:'t -> 't -> 't) (symbol: string) (bound:'u) (range:bool) (body:'t) = formula<'t>
    /// Generic quantifier with product semantics.
    let product<'t,'u> (op:'t -> 't -> 't) (symbol: string) (bound:'u) (range:bool) (body:'t) = formula<'t>

    // Functions
    let func<'s, 't> = fun (_:'s) -> Unchecked.defaultof<'t>
    
    let func2<'r, 's, 't> = fun (_:'r) (_:'s) -> Unchecked.defaultof<'t>

    (* Formula patterns *)          
    let (|True|_|) =
        function
        | Bool true
        | ValueWithName(_, _, "T") -> Some()
        | _ -> None

    
    let (|False|_|) =
        function
        | Bool false
        | ValueWithName(_, _, "F") -> Some()
        | _ -> None

    let (|Equals|_|) = 
         function
         | SpecificCall <@@ (=) @@> (None,_,l::r::[]) when l.Type = r.Type -> Some(l, r)
         | _ -> None
   
    let (|Not|_|) =
        function
        | SpecificCall <@@ not @@> (None,_,l::[]) -> Some l
        | _ -> None

    let (|NotEquals|_|) =
         function
         | SpecificCall <@@ (<>) @@> (None,_,l::r::[]) -> Some (l, r)
         | _ -> None

    let (|And|_|) =
        function
        | SpecificCall <@@ (&&) @@> (None,_,l::r::[]) -> Some (l, r)
        | IfThenElse(l, r, elseBranch) ->
            match elseBranch with 
            | ValueWithName(_, _, "F") -> None
            | Bool false -> Some(l, r)
            | _ -> None
        | _ -> None
    
    let (|Or|_|) =
        function        
        | SpecificCall <@@ (||) @@> (None,_,l::r::[]) -> Some (l, r)
        | IfThenElse(l, thenBranch, r) ->
            match thenBranch with
            | ValueWithName(_, _, "T") -> None
            | Bool true -> Some(l, r)
            | _ -> None
        | _ -> None

    let (|Implies|_|) =
        function
        | SpecificCall <@@ (===>) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Conseq|_|) =
        function
        | SpecificCall <@@ (<===) @@> (None,_,l::r::[]) -> Some (l, r)
        | _ -> None

    let (|Argument|_|) =
        let rec get_conjuncts =
            function
            | And(L, R) as c -> [c] @ get_conjuncts L @ get_conjuncts R
            | expr  -> [expr]
        function
        | Implies(a, c) -> (a, c, get_conjuncts a) |> Some
        | _ -> None

    let (|Binary|_|) (op:Expr<'t->'t->'u>) =
        function
        | SpecificCall op (None,_,l::r::[]) when l.Type = typeof<'t> && r.Type = typeof<'t> -> Some (l,r)
        | And(l,r ) when (getFuncInfo op).Name = "op_BooleanAnd" -> Some(l, r)
        | Or(l,r ) when (getFuncInfo op).Name = "op_BooleanOr" -> Some(l, r)
        | _ -> None

    let (|Binary'|_|) (op:Expr<'t->'u->'u>) =
        function
        | SpecificCall op (None,_,l::r::[]) when l.Type = typeof<'t> && r.Type = typeof<'u> -> Some (l,r)
        | _ -> None

    let (|Unary|_|) (op:Expr<'t->'u>) =
        function
        | SpecificCall op (None,_,r::[]) when r.Type = typeof<'t> -> Some r
        | _ -> None

    let (|BinaryCall|_|) =
        function
        | Call(_,_,l::r::[]) when l.Type = r.Type -> Some (l,r)
        | _ -> None

    let (|UnaryCall|_|)  =
        function
        | Call(_,_,r::[]) -> Some r
        | _ -> None

    let (|Val|_|) (v:'t) =
        function
        | Value(z, t) when (t = typeof<'t>) && ((z :?> 't) = v) -> Some (Expr.Value(v))
        | _ -> None

    let (|Add|_|) =
        function
        | Call(None, Op "op_Addition" ,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Subtract|_|) =
        function
        | Call(None, Op "op_Subtraction" ,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Multiply|_|) =
        function
        | Call(None, Op "op_Multiply" ,l::r::[]) -> Some(l, r)
        | _ -> None

    let (|Divide|_|) =
        function
        | Call(None, Op "op_Division" ,l::r::[]) -> Some(l, r)
        | _ -> None
    
    let (|Negate|_|) =
        function
        | Call(None, Op "op_UnaryNegation" ,r::[]) -> Some r 
        | _ -> None

    let (|LessThan|_|) =
         function
         | SpecificCall <@@ (<) @@> (None,_,l::r::[]) -> Some (l, r)
         | _ -> None

    let (|Range|_|) =
        function
        | SpecificCall <@@ (..) @@> (None,_,l::r::[]) -> Some(l,r)
        | _ -> None

    let (|Sequence|_|) =
        function
        | Call(None, Op "CreateSequence", Call(None, Op "Delay", Lambda(_, Call(None, Op "Singleton", v))::[])::[]) 
            ->  v |> List.map expand |> Some
        | Call (None, Op "InitializeInfinite", e) -> e |> List.map expand |> Some
        | List l -> l |> List.map expand |> Some
        | NewArray(_, a) -> a |> List.map expand |> Some
        | _ -> None

    let (|BoundVars|_|) =
        function
        | NewTuple(bound) -> bound |> List.map get_vars |> List.concat |> Some
        | ExprShape.ShapeVar bound -> [bound] |> Some
        | ValueWithName(v,t,n) -> let bound = Var(n, t) in [bound] |> Some 
        | Coerce(ValueWithName(v,t,n), ct) when ct = typeof<obj> -> let bound = Var(n, t) in [bound] |> Some
        | _ -> None

    let (|Index|_|) =
        function
        | Call(Some h, mi,  BoundVars(bound)::[]) when mi.Name = "Item" -> Some(h, bound)
        | _ -> None

    let (|Predicate|_|) =
        function
        | Value(_, t) as v when t = typeof<bool> -> Some v
        | Var v as s when v.Type = typeof<_->bool> -> Some s
        | Lambda(_, b) as l when b.Type = typeof<bool> -> Some l
        | _ -> None

    let (|ForAll|_|) =
        function
        | Call(None, mi, BoundVars(bound)::range::body::[]) when mi.Name = "forall_expr" -> Some(<@@ forall_expr @@>, bound, range, body)
        | _ -> None

    let (|Exists|_|) =
        function
        | Call(None, mi, BoundVars(bound)::range::body::[]) when mi.Name = "exists_expr" -> Some(<@@ exists_expr @@>, bound, range, body)
        | _ -> None

    let (|Sum|_|) =
        function
        | Call(None, mi, op::Value(_, t)::BoundVars(bound)::range::body::[]) when mi.Name = "sum" && t = typeof<string> -> Some(op, bound, range, body)
        | _ -> None

    let (|Product|_|) =
        function
        | Call(None, mi, op::Value(_, t)::BoundVars(bound)::range::body::[]) when mi.Name = "product" && t = typeof<string> -> Some(op, bound, range, body)
        | _ -> None

    let (|Quantifier|_|) =
        function
        | Sum x
        | Product x
        | ForAll x
        | Exists x -> let (op, bound, range, body) = x in Some (op, bound, range, body)
        | _ -> None