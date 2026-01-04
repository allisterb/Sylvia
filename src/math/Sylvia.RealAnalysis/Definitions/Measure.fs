namespace Sylvia

type Measure<'t when 't : equality> = SigmaAlgebra<'t> -> real
