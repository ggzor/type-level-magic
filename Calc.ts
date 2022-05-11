// type Calc<T> = Eval<Parse<Lex<T>>>;

type Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";

type Unfold<T extends string> = T extends "" ? [] : [T];

type Lex<T, Prev extends string = ""> = T extends ""
  ? Unfold<Prev>
  : T extends `+${infer Rest}`
  ? [...Unfold<Prev>, "+", ...Lex<Rest>]
  : T extends `*${infer Rest}`
  ? [...Unfold<Prev>, "*", ...Lex<Rest>]
  : T extends `(${infer Rest}`
  ? [...Unfold<Prev>, "(", ...Lex<Rest>]
  : T extends `)${infer Rest}`
  ? [...Unfold<Prev>, ")", ...Lex<Rest>]
  : T extends ` ${infer Rest}`
  ? [...Unfold<Prev>, ...Lex<Rest>]
  : T extends `${infer First}${infer Rest}`
  ? First extends Digit
    ? Lex<Rest, `${Prev}${First}`>
    : never
  : never;

type AsPeano<T, Acc extends 0[] = []> = T extends `${Acc["length"]}`
  ? Acc
  : AsPeano<T, [0, ...Acc]>;

type AsNum<T extends any[]> = T["length"];

type PNumber<T> = T extends [infer Num, ...infer Rest]
  ? [AsPeano<Num>, Rest]
  : never;

type PGroup<T> = T extends ["(", ...infer Rest]
  ? PSum<Rest> extends [infer Inner, [")", ...infer Rest2]]
    ? [Inner, Rest2]
    : never
  : never;

type PFactor<T> = T extends [infer First, ...infer _]
  ? First extends "("
    ? PGroup<T>
    : PNumber<T>
  : never;

type PMult<T> = PFactor<T> extends [infer Left, infer Rest]
  ? Rest extends ["*", ...infer Rest2]
    ? PMult<Rest2> extends [infer Right, infer Rest3]
      ? [["*", Left, Right], Rest3]
      : never
    : [Left, Rest]
  : never;

type PSum<T> = PMult<T> extends [infer Left, infer Rest]
  ? Rest extends ["+", ...infer Rest2]
    ? PSum<Rest2> extends [infer Right, infer Rest3]
      ? [["+", Left, Right], Rest3]
      : never
    : [Left, Rest]
  : never;

type Parse<T> = PSum<T> extends [infer Value, []] ? Value : never;

type Sum<A extends 0[], B extends 0[]> = [...A, ...B];
type Prod<A extends 0[], B extends any[]> = B extends []
  ? []
  : B extends [0, ...infer Rest]
  ? Sum<A, Prod<A, Rest>>
  : never;

type Eval<T> = T extends 0[]
  ? T
  : T extends ["+", infer Left, infer Right]
  ? Sum<Eval<Left>, Eval<Right>>
  : T extends ["*", infer Left, infer Right]
  ? Prod<Eval<Left>, Eval<Right>>
  : never;

type Calc<T> = AsNum<Eval<Parse<Lex<T>>>>;

type Result = Calc<"10 * 4 + 2 * 5">;
