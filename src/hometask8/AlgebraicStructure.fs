module AlgebraicStructure


type Monoid<'t> =
    val Sum: 't -> 't -> 't
    val Neutral: 't 
    new (x, y) = { Sum = x; Neutral = y }

type SemiRing<'t>  =
    val Monoid: Monoid<'t>
    val NeutralMul: 't
    val Mul: 't -> 't -> 't
    new (mon, mul, neutral) = { Monoid = mon; Mul = mul; NeutralMul = neutral }

type AlgebraicStruct<'t> =
    | Monoid of Monoid<'t>
    | SemiRing of SemiRing<'t>

    static member sumOp (algStr: AlgebraicStruct<'t>) =
        match algStr with
        | Monoid x -> x.Sum
        | SemiRing x -> x.Monoid.Sum
        

    static member mulOp (algStr: AlgebraicStruct<'t>) =
        match algStr with
        | Monoid x -> x.Sum 
        | SemiRing x -> x.Mul

    static member neutral (algStr: AlgebraicStruct<'t>) =
        match algStr with
        | Monoid x -> x.Neutral
        | SemiRing x -> x.Monoid.Neutral
