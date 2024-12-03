type ToString<Lit> =
  Lit extends (number | string | boolean) ? `${Lit}` : never;

// Pairs
type Fst<A> = A extends [infer I, any] ? I : never;
type Snd<A> = A extends [any, infer I] ? I : never;

// Binary logic
type Switch<A, T0, T1> =
  A extends 0 ? T0 :
  A extends 1 ? T1 :
  never;

type Not<A>    = Switch<A, 1, 0>;
type And<A, B> = Switch<A, 0, B>;
type Or <A, B> = Switch<A, B, 1>;
type Xor<A, B> = Switch<A, B, Not<B>>;

type BinAdd1<A, B, C> = [
    Xor<A, Xor<B, C>>,
    Switch<A, And<B, C>, Or<B, C>>,
];

// Cons lists (terminated by [])
type ListSwitch<A, Tempty, Tcons> =
  A extends [] ? Tempty :
  A extends [any, any] ? Tcons :
  never;
type ListToUnion<A> = A extends [] ? never : Fst<A> | ListToUnion<Snd<A>>;
type ListCat<A, B> = A extends [] ? B : [Fst<A>, ListCat<Snd<A>, B>];
type ListRCat<A, B> = A extends [] ? B : ListRCat<Snd<A>, [Fst<A>, B]>;
type ListRev<A> = ListRCat<A, []>;
type NumHead<A> = A extends [] ? 0  : Fst<A>;
type NumTail<A> = A extends [] ? [] : Snd<A>;
type ListIndex<L, A> = BinNonZero<A> extends 0 ? Fst<L> : ListIndex<Snd<L>, BinPred<A>>;
type ListRIndex<L, X> = X extends Fst<L> ? [] : BinSucc<ListRIndex<Snd<L>, X>>;
type ListLength<L> = L extends [] ? [] : BinSucc<ListLength<Snd<L>>>;
type ListMapToStr<L> = L extends [] ? [] : [ToString<Fst<L>>, ListMapToStr<Snd<L>>];

// Arbitrary-size binary numbers (as little-endian cons lists)
type BinSucc<A> = Switch<NumHead<A>, [1, NumTail<A>], [0, BinSucc<NumTail<A>>]>;
type BinPred<A> = Switch<NumHead<A>, [1, BinPred<NumTail<A>>], [0, NumTail<A>]>;
type BinNonZero<A> = A extends [] ? 0 : Or<Fst<A>, BinNonZero<Snd<A>>>;

type __BinAdd<A, B, R> = [Fst<R>, _BinAdd<A, B, Snd<R>>];
type _BinAdd<A, B, Carry> = [A, B, Carry] extends [[], [], 0] ? [] :
  __BinAdd<NumTail<A>, NumTail<B>, BinAdd1<NumHead<A>, NumHead<B>, Carry>>;
type BinAdd<A, B> = _BinAdd<A, B, 0>;

type BinMul<A, B> = A extends [] ? [] :
  BinAdd<Switch<Fst<A>, [], B>, [0, BinMul<Snd<A>, B>]>;

// decimal <-> binary
type DecDigits = [0, [1, [2, [3, [4, [5, [6, [7, [8, [9, []]]]]]]]]]];
type Bin2Dec1<A> = ListIndex<DecDigits, A>;
type Dec2Bin1<A> = ListRIndex<DecDigits, A>;
type TimesTen<A> = [0, BinAdd<A, [0, [0, [0, A]]]>];
type Dec2Bin<A> = A extends [] ? [] : BinAdd<
  ListRIndex<DecDigits, Fst<A>>,
  BinMul<Dec2Bin<Snd<A>>, ListLength<DecDigits>>
>;

// decimal <-> text

type ReadDec<Digs, I extends string> =
  I extends `0${infer R}` ? ReadDec<[0, Digs], R> :
  I extends `1${infer R}` ? ReadDec<[1, Digs], R> :
  I extends `2${infer R}` ? ReadDec<[2, Digs], R> :
  I extends `3${infer R}` ? ReadDec<[3, Digs], R> :
  I extends `4${infer R}` ? ReadDec<[4, Digs], R> :
  I extends `5${infer R}` ? ReadDec<[5, Digs], R> :
  I extends `6${infer R}` ? ReadDec<[6, Digs], R> :
  I extends `7${infer R}` ? ReadDec<[7, Digs], R> :
  I extends `8${infer R}` ? ReadDec<[8, Digs], R> :
  I extends `9${infer R}` ? ReadDec<[9, Digs], R> :
  [Digs, I];

type ParseDec<I extends string> =
  ReadDec<[], I> extends [[infer Xh, infer Xt], infer R] ?
    [Dec2Bin<[Xh, Xt]>, R] : false;

type DecToString1<Dig> = ListIndex<ListMapToStr<DecDigits>, Dec2Bin1<Dig>>;
type DecToString<Digs> = Digs extends [] ? '' :
  `${DecToString<Snd<Digs>>}${DecToString1<Fst<Digs>>}`;

// AoC

type Part1<I> =
  I extends `${infer _}mul(${infer I}` ?
  ParseDec<I> extends [infer A, infer I] ?
  I extends `,${infer I}` ?
  ParseDec<I> extends [infer B, infer I] ?
  I extends `)${infer I}` ?
  BinAdd<BinMul<A, B>, Part1<I>>
  : Part1<I> : Part1<I> : Part1<I> : Part1<I> : [];

type Output = DecToString<Part1<Input>>;
type Input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
