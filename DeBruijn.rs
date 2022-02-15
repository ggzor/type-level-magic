// #![recursion_limit = "2048"]
#![allow(dead_code)]
use std::{any::type_name, marker::PhantomData};

// Naturals

enum Z {}
struct S<N>(PhantomData<N>);

trait Nat {}
impl Nat for Z {}
impl<N: Nat> Nat for S<N> {}

// Nat shorthands
type N0 = Z;
type N1 = S<Z>;
type N2 = S<N1>;
type N3 = S<N2>;
type N4 = S<N3>;
type N5 = S<N4>;
type N6 = S<N5>;
type N7 = S<N6>;
type N8 = S<N7>;
type N9 = S<N8>;
type N10 = S<N9>;

// Decrement
trait Dec {
    type Output;
}

impl<N> Dec for S<N> {
    type Output = N;
}

// Sum
trait Sum<Rhs> {
    type Output;
}

impl<Rhs> Sum<Rhs> for Z {
    type Output = Rhs;
}

impl<Rhs, N: Sum<Rhs>> Sum<Rhs> for S<N> {
    type Output = S<<N as Sum<Rhs>>::Output>;
}

// Conditional less than
trait IfLess<Rhs, True, False> {
    type Output;
}

impl<True, False> IfLess<Z, True, False> for Z {
    type Output = False;
}

impl<True, False, N> IfLess<Z, True, False> for S<N> {
    type Output = False;
}

impl<Rhs, True, False> IfLess<S<Rhs>, True, False> for Z {
    type Output = True;
}

impl<Rhs, True, False, N> IfLess<S<Rhs>, True, False> for S<N>
where
    N: IfLess<Rhs, True, False>,
{
    type Output = <N as IfLess<Rhs, True, False>>::Output;
}

// Conditional equality

trait IfEq<Rhs, True, False> {
    type Output;
}

impl<True, False> IfEq<Z, True, False> for Z {
    type Output = True;
}

impl<Rhs, True, False> IfEq<S<Rhs>, True, False> for Z {
    type Output = False;
}

impl<True, False, N> IfEq<Z, True, False> for S<N> {
    type Output = False;
}

impl<Rhs, True, False, N> IfEq<S<Rhs>, True, False> for S<N>
where
    N: IfEq<Rhs, True, False>,
{
    type Output = <N as IfEq<Rhs, True, False>>::Output;
}

// DeBruijn
struct Var<N>(PhantomData<N>);
struct Lam<T>(PhantomData<T>);
struct App<T1, T2>(PhantomData<T1>, PhantomData<T2>);

/*
shift d c (V k) = if k < c then V k else V (k + d)
shift d c (L t1) = L (shift d (c + 1) t1)
shift d c (App t1 t2) = App (shift d c t1) (shift d c t2)
*/

enum MinusOne {}

trait Shift<D, C> {
    type Output;
}

// Special case
impl<C> Shift<MinusOne, C> for Var<Z> {
    type Output = Var<Z>;
}

impl<C, K> Shift<MinusOne, C> for Var<K>
where
    K: Dec + IfLess<C, Var<K>, Var<<K as Dec>::Output>>,
{
    type Output = <K as IfLess<C, Var<K>, Var<<K as Dec>::Output>>>::Output;
}

impl<D: Nat, C, K> Shift<D, C> for Var<K>
where
    K: Sum<D> + IfLess<C, Var<K>, Var<<K as Sum<D>>::Output>>,
{
    type Output = <K as IfLess<C, Var<K>, Var<<K as Sum<D>>::Output>>>::Output;
}

impl<D, C, T1> Shift<D, C> for Lam<T1>
where
    T1: Shift<D, S<C>>,
{
    type Output = Lam<<T1 as Shift<D, S<C>>>::Output>;
}

impl<D, C, T1, T2> Shift<D, C> for App<T1, T2>
where
    T1: Shift<D, C>,
    T2: Shift<D, C>,
{
    type Output = App<<T1 as Shift<D, C>>::Output, <T2 as Shift<D, C>>::Output>;
}

/*
sub j s (V k) = if k == j then s else V k
sub j s (L t1) = L (sub (j + 1) (shift 1 0 s) t1)
sub j s (App t1 t2) = App (sub j s t1) (sub j s t2)
*/

trait Subst<J, S_> {
    type Output;
}

impl<J, S_, K> Subst<J, S_> for Var<K>
where
    K: IfEq<J, S_, Var<K>>,
{
    type Output = <K as IfEq<J, S_, Var<K>>>::Output;
}

impl<J, S_, T1> Subst<J, S_> for Lam<T1>
where
    S_: Shift<N1, N0>,
    T1: Subst<S<J>, <S_ as Shift<N1, N0>>::Output>,
{
    type Output = Lam<<T1 as Subst<S<J>, <S_ as Shift<N1, N0>>::Output>>::Output>;
}

impl<J, S_, T1, T2> Subst<J, S_> for App<T1, T2>
where
    T1: Subst<J, S_>,
    T2: Subst<J, S_>,
{
    type Output = App<<T1 as Subst<J, S_>>::Output, <T2 as Subst<J, S_>>::Output>;
}

/*
normal (V _) = True
normal (L t) = normal t
normal (App (L _) _) = False
normal (App t1 t2) = normal t1 && normal t2
*/

trait IfNormal<True, False> {
    type Output;
}

impl<True, False, K> IfNormal<True, False> for Var<K> {
    type Output = True;
}

impl<True, False, T1> IfNormal<True, False> for Lam<T1>
where
    T1: IfNormal<True, False>,
{
    type Output = <T1 as IfNormal<True, False>>::Output;
}

impl<True, False, T12, T2> IfNormal<True, False> for App<Lam<T12>, T2> {
    type Output = False;
}

impl<True, False, T1, T2> IfNormal<True, False> for App<Var<T1>, T2>
where
    T2: IfNormal<True, False>,
{
    type Output = <Var<T1> as IfNormal<<T2 as IfNormal<True, False>>::Output, False>>::Output;
}

impl<True, False, T12, T22, T2> IfNormal<True, False> for App<App<T12, T22>, T2>
where
    App<T12, T22>: IfNormal<<T2 as IfNormal<True, False>>::Output, False>,
    T2: IfNormal<True, False>,
{
    type Output = <App<T12, T22> as IfNormal<<T2 as IfNormal<True, False>>::Output, False>>::Output;
}

/*
beta (App (L t12) v2) = Just $ shift (-1) 0 (sub 0 (shift 1 0 v2) t12)
beta (App t1 t2) | normal t1 = App t1 <$> beta t2
beta (App t1 t2) = App <$> beta t1 <*> pure t2
beta (L t1) = L <$> beta t1
beta _ = Nothing
*/

trait Beta {
    type Output;
}

impl<T12, T2> Beta for App<Lam<T12>, T2>
where
    T12: Subst<N0, <T2 as Shift<N1, N0>>::Output>,
    T2: Shift<N1, N0>,
    <T12 as Subst<Z, <T2 as Shift<S<Z>, Z>>::Output>>::Output: Shift<MinusOne, Z>,
{
    type Output =
        <<T12 as Subst<N0, <T2 as Shift<N1, N0>>::Output>>::Output as Shift<MinusOne, N0>>::Output;
}

// Duplicated code from IsNormal
impl<T1, T2> Beta for App<Var<T1>, T2>
where
    T2: Beta,
{
    type Output = App<Var<T1>, <T2 as Beta>::Output>;
}

struct NormFirst<A, B>(PhantomData<(A, B)>);
struct NormSecond<A, B>(PhantomData<(A, B)>);

trait ApplyNorm {
    type Output;
}

impl<A: Beta, B> ApplyNorm for NormFirst<A, B> {
    type Output = App<<A as Beta>::Output, B>;
}

impl<A, B: Beta> ApplyNorm for NormSecond<A, B> {
    type Output = App<A, <B as Beta>::Output>;
}

impl<T11, T12, T2> Beta for App<App<T11, T12>, T2>
where
    App<T11, T12>: IfNormal<NormSecond<App<T11, T12>, T2>, NormFirst<App<T11, T12>, T2>>,
    <App<T11, T12> as IfNormal<NormSecond<App<T11, T12>, T2>, NormFirst<App<T11, T12>, T2>>>::Output: ApplyNorm
{
    type Output = <<App<T11, T12> as IfNormal<
        NormSecond<App<T11, T12>, T2>,
        NormFirst<App<T11, T12>, T2>,
    >>::Output as ApplyNorm>::Output;
}

impl<T> Beta for Lam<T>
where
    T: Beta,
{
    type Output = Lam<<T as Beta>::Output>;
}

// Full beta reduction

struct Normal<T>(PhantomData<T>);
struct NotNormal<T>(PhantomData<T>);

trait Step {
    type Output;
}

trait FullBeta {
    type Output;
}

impl<T> Step for Normal<T> {
    type Output = T;
}

impl<T> Step for NotNormal<T>
where
    T: Beta,
    <T as Beta>::Output: FullBeta,
{
    type Output = <<T as Beta>::Output as FullBeta>::Output;
}

impl<T> FullBeta for T
where
    T: IfNormal<Normal<T>, NotNormal<T>>,
    <T as IfNormal<Normal<T>, NotNormal<T>>>::Output: Step,
{
    type Output = <<T as IfNormal<Normal<T>, NotNormal<T>>>::Output as Step>::Output;
}

type FullReduce<T> = <T as FullBeta>::Output;

// End of definitions
fn print_type<T>(label: &str) {
    println!(
        "{}:\n  {}\n",
        label,
        type_name::<T>().replace("DeBruijn::", "")
    );
}

// Tests
type LId = Lam<Var<N0>>;
type LTrue = Lam<Lam<Var<N1>>>;
type LFalse = Lam<Lam<Var<N0>>>;
type LNot = Lam<Lam<Lam<App<App<Var<N2>, Var<N0>>, Var<N1>>>>>;
type LAnd = Lam<Lam<App<App<Var<N1>, Var<N0>>, Var<N1>>>>;
type LOr = Lam<Lam<App<App<Var<N1>, Var<N1>>, Var<N0>>>>;
type LIf = Lam<Lam<Lam<App<App<Var<N2>, Var<N1>>, Var<N0>>>>>;

type LZero = Lam<Lam<Var<N0>>>;

// succ = \n.\f.\x.f (n f x)
type LSuc = Lam<Lam<Lam<App<Var<N1>, App<App<Var<N2>, Var<N1>>, Var<N0>>>>>>;
type L0 = LZero;
type L1 = App<LSuc, L0>;
type L2 = App<LSuc, L1>;
type L3 = App<LSuc, L2>;
type L4 = App<LSuc, L3>;
type L5 = App<LSuc, L4>;

// plus = \m.\n.\f.\x. m f (n f x)
type LPlus = Lam<Lam<Lam<Lam<App<App<Var<N3>, Var<N1>>, App<App<Var<N2>, Var<N1>>, Var<N0>>>>>>>;
// mult = \m.\n.\f.\x.m (n f) x
type LMult = Lam<Lam<Lam<Lam<App<App<Var<N3>, App<Var<N2>, Var<N1>>>, Var<N0>>>>>>;
// pred = \n.\f.\x.n (\g.\h.h (g f)) (\u.x) (\u.u)
type LPred = Lam<
    Lam<
        Lam<
            App<
                App<App<Var<N2>, Lam<Lam<App<Var<N0>, App<Var<N1>, Var<N3>>>>>>, Lam<Var<N1>>>,
                Lam<Var<N0>>,
            >,
        >,
    >,
>;
// minus = \m.\n.n pred m
type LMinus = Lam<Lam<App<App<Var<N0>, LPred>, Var<N1>>>>;

// isZero = \n.n (\x.false) true
type LIsZero = Lam<App<App<Var<N0>, Lam<LFalse>>, LTrue>>;

// pair = \x.\y.\p.p x y
type LPair = Lam<Lam<Lam<App<App<Var<N0>, Var<N2>>, Var<N1>>>>>;
// first = \p.p (\x.\y.x)
type LFirst = Lam<App<Var<N0>, Lam<Lam<Var<N1>>>>>;
// second = \p.p (\x.\y.y)
type LSecond = Lam<App<Var<N0>, Lam<Lam<Var<N0>>>>>;

// Y = \f. (\x. f (x x)) (\x. f (x x))
type Y =
    Lam<App<Lam<App<Var<N1>, App<Var<N0>, Var<N0>>>>, Lam<App<Var<N1>, App<Var<N0>, Var<N0>>>>>>;

// fact' = \f.\x. if (isZero x) 1 (mult x (f (pred x)))
type LFactInner = Lam<
    Lam<
        App<
            App<App<LIf, App<LIsZero, Var<N0>>>, L1>,
            App<App<LMult, Var<N0>>, App<Var<N1>, App<LPred, Var<N0>>>>,
        >,
    >,
>;

// fact = Y fact'
type LFact = App<Y, LFactInner>;

fn main() {
    type Booleans1 = App<LNot, LFalse>;
    print_type::<FullReduce<Booleans1>>("Booleans -> not false == true");

    type Booleans2 = App<App<App<LIf, LFalse>, LTrue>, LId>;
    print_type::<FullReduce<Booleans2>>("Booleans -> if false true id == id");

    type Booleans3 = App<App<LAnd, LTrue>, App<App<LOr, LFalse>, LTrue>>;
    print_type::<FullReduce<Booleans3>>("Booleans -> and true (or false true) == true");

    type Naturals1 = App<App<LPlus, L5>, L4>;
    print_type::<FullReduce<Naturals1>>("Naturals -> 5 + 4 == 9 (9 App's)");

    type Naturals2 = App<App<LMult, L3>, L4>;
    print_type::<FullReduce<Naturals2>>("Naturals -> 3 * 4 == 12 (12 App's)");

    type Naturals3 = App<LPred, L5>;
    print_type::<FullReduce<Naturals3>>("Naturals -> 5 - 1 == 4  (4 App's)");

    type Naturals4 = App<App<LMinus, L5>, L2>;
    print_type::<FullReduce<Naturals4>>("Naturals -> 5 - 2 == 3  (3 App's)");

    type Naturals5 = App<LIsZero, L3>;
    print_type::<FullReduce<Naturals5>>("Naturals -> isZero 3 == false");

    type ExamplePair = App<App<LPair, App<App<LPair, L1>, L2>>, L3>;
    type Pairs = App<LSecond, App<LFirst, ExamplePair>>;
    print_type::<FullReduce<Pairs>>("Pairs -> snd (fst ((1, 2), 3)) == 2 (2 App's)");

    // This is the only test that requires tons of resources:
    //  - it will eat up your RAM (tested with 16GB of RAM)
    //  - requires recursion_limit="2048"
    //  - takes about 2 minutes @4.2Ghz
    // But it works!!!

    // type Recursion = App<LFact, L3>;
    // print_type::<FullReduce<Recursion>>("Recursion -> 3! == 6 (6 App's)");
}
