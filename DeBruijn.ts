type CountTo<N extends number, R extends number[] = []> = N extends R["length"]
  ? R
  : CountTo<N, [0, ...R]>;

type Inc<N extends number[]> = [0, ...N];
type Dec<N extends number[]> = N extends [infer _, ...infer Rest]
  ? Rest
  : never;
type Sum<N extends number[], M extends number[]> = [...N, ...M];

type IfLess<A, B, True, False> = B extends []
  ? False
  : A extends []
  ? True
  : A extends [infer _, ...infer RA]
  ? B extends [infer _, ...infer RB]
    ? IfLess<RA, RB, True, False>
    : never
  : never;

type IfEq<A, B, True, False> = A extends []
  ? B extends []
    ? True
    : False
  : B extends []
  ? False
  : A extends [infer _, ...infer RA]
  ? B extends [infer _, ...infer RB]
    ? IfEq<RA, RB, True, False>
    : never
  : never;

type AsDec<N extends number[]> = N["length"];

type TD<Term> = Term extends number
  ? CountTo<Term>
  : Term extends [infer Inner]
  ? { inner: TD<Inner> }
  : Term extends [infer Left, infer Right]
  ? { left: TD<Left>; right: TD<Right> }
  : never;

type UnTD<Term> = Term extends { inner: infer Inner }
  ? [UnTD<Inner>]
  : Term extends { left: infer Left; right: infer Right }
  ? [UnTD<Left>, UnTD<Right>]
  : Term extends number[]
  ? AsDec<Term>
  : never;

/*
shift d c (V k) = if k < c then V k else V (k + d)
shift d c (L t1) = L (shift d (c + 1) t1)
shift d c (App t1 t2) = App (shift d c t1) (shift d c t2)
*/

type Shift<
  D extends number[] | -1,
  C extends number[],
  Term
> = Term extends number[]
  ? IfLess<Term, C, Term, D extends number[] ? Sum<Term, D> : Dec<Term>>
  : Term extends { inner: infer Inner }
  ? { inner: Shift<D, Inc<C>, Inner> }
  : Term extends { left: infer Left; right: infer Right }
  ? { left: Shift<D, C, Left>; right: Shift<D, C, Right> }
  : never;

/*
sub j s (V k) = if k == j then s else V k
sub j s (L t1) = L (sub (j + 1) (shift 1 0 s) t1)
sub j s (App t1 t2) = App (sub j s t1) (sub j s t2)
*/

type Subst<J extends number[], S, Term> = Term extends number[]
  ? IfEq<Term, J, S, Term>
  : Term extends { inner: infer Inner }
  ? { inner: Subst<Inc<J>, Shift<CountTo<1>, CountTo<0>, S>, Inner> }
  : Term extends { left: infer Left; right: infer Right }
  ? { left: Subst<J, S, Left>; right: Subst<J, S, Right> }
  : never;

/*
normal (V _) = True
normal (L t) = normal t
normal (App (L _) _) = False
normal (App t1 t2) = normal t1 && normal t2
*/

type IfNormal<Term, True, False> = Term extends number[]
  ? True
  : Term extends { inner: infer Inner }
  ? IfNormal<Inner, True, False>
  : Term extends { left: { inner: infer _ }; right: infer _ }
  ? False
  : Term extends { left: infer Left; right: infer Right }
  ? IfNormal<Left, IfNormal<Right, True, False>, False>
  : never;

/*
beta (App (L t12) v2) = Just $ shift (-1) 0 (sub 0 (shift 1 0 v2) t12)
beta (App t1 t2) | normal t1 = App t1 <$> beta t2
beta (App t1 t2) = App <$> beta t1 <*> pure t2
beta (L t1) = L <$> beta t1
beta _ = Nothing
*/

type Beta<Term> = Term extends { left: { inner: infer T12 }; right: infer V2 }
  ? Shift<
      -1,
      CountTo<0>,
      Subst<CountTo<0>, Shift<CountTo<1>, CountTo<0>, V2>, T12>
    >
  : Term extends { inner: infer Inner }
  ? { inner: Beta<Inner> }
  : Term extends { left: infer Left; right: infer Right }
  ? IfNormal<
      Left,
      { left: Left; right: Beta<Right> },
      { left: Beta<Left>; right: Right }
    >
  : never;

type FullBeta<Term> = IfNormal<Term, true, false> extends true
  ? Term
  : FullBeta<Beta<Term>>;

type LId = [0];
type LTrue = [[1]];
type LFalse = [[0]];
type LNot = [[[[[2, 0], 1]]]];
type LAnd = [[[[[[3, [[2, 1], 0]], 0]]]]];
type LOr = [[[[[[3, 1], [[2, 1], 0]]]]]];

type Test = [[LAnd, LFalse], LFalse];

type Result = UnTD<FullBeta<TD<Test>>>;
