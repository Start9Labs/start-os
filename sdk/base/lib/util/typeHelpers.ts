import * as T from "../types"

// prettier-ignore
export type FlattenIntersection<T> = 
T extends ArrayLike<any> ? T :
T extends object ? {} & {[P in keyof T]: T[P]} :
 T;

export type _<T> = FlattenIntersection<T>

export const isKnownError = (e: unknown): e is T.KnownError =>
  e instanceof Object && ("error" in e || "error-code" in e)

declare const affine: unique symbol

export type Affine<A> = { [affine]: A }

type NeverPossible = { [affine]: string }
export type NoAny<A> = NeverPossible extends A
  ? keyof NeverPossible extends keyof A
    ? never
    : A
  : A

type CapitalLetters =
  | "A"
  | "B"
  | "C"
  | "D"
  | "E"
  | "F"
  | "G"
  | "H"
  | "I"
  | "J"
  | "K"
  | "L"
  | "M"
  | "N"
  | "O"
  | "P"
  | "Q"
  | "R"
  | "S"
  | "T"
  | "U"
  | "V"
  | "W"
  | "X"
  | "Y"
  | "Z"

type Numbers = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

type CapitalChars = CapitalLetters | Numbers

export type ToKebab<S extends string> = S extends string
  ? S extends `${infer Head}${CapitalChars}${infer Tail}` // string has a capital char somewhere
    ? Head extends "" // there is a capital char in the first position
      ? Tail extends ""
        ? Lowercase<S> /*  'A' */
        : S extends `${infer Caps}${Tail}` // tail exists, has capital characters
          ? Caps extends CapitalChars
            ? Tail extends CapitalLetters
              ? `${Lowercase<Caps>}-${Lowercase<Tail>}` /* 'AB' */
              : Tail extends `${CapitalLetters}${string}`
                ? `${ToKebab<Caps>}-${ToKebab<Tail>}` /* first tail char is upper? 'ABcd' */
                : `${ToKebab<Caps>}${ToKebab<Tail>}` /* 'AbCD','AbcD',  */ /* TODO: if tail is only numbers, append without underscore */
            : never /* never reached, used for inference of caps */
          : never
      : Tail extends "" /* 'aB' 'abCD' 'ABCD' 'AB' */
        ? S extends `${Head}${infer Caps}`
          ? Caps extends CapitalChars
            ? Head extends Lowercase<Head> /* 'abcD' */
              ? Caps extends Numbers
                ? // Head exists and is lowercase, tail does not, Caps is a number, we may be in a sub-select
                  // if head ends with number, don't split head an Caps, keep contiguous numbers together
                  Head extends `${string}${Numbers}`
                  ? never
                  : // head does not end in number, safe to split. 'abc2' -> 'abc-2'
                    `${ToKebab<Head>}-${Caps}`
                : `${ToKebab<Head>}-${ToKebab<Caps>}` /* 'abcD' 'abc25' */
              : never /* stop union type forming */
            : never
          : never /* never reached, used for inference of caps */
        : S extends `${Head}${infer Caps}${Tail}` /* 'abCd' 'ABCD' 'AbCd' 'ABcD' */
          ? Caps extends CapitalChars
            ? Head extends Lowercase<Head> /* is 'abCd' 'abCD' ? */
              ? Tail extends CapitalLetters /* is 'abCD' where Caps = 'C' */
                ? `${ToKebab<Head>}-${ToKebab<Caps>}-${Lowercase<Tail>}` /* aBCD Tail = 'D', Head = 'aB' */
                : Tail extends `${CapitalLetters}${string}` /* is 'aBCd' where Caps = 'B' */
                  ? Head extends Numbers
                    ? never /* stop union type forming */
                    : Head extends `${string}${Numbers}`
                      ? never /* stop union type forming */
                      : `${Head}-${ToKebab<Caps>}-${ToKebab<Tail>}` /* 'aBCd' => `${'a'}-${Lowercase<'B'>}-${ToSnake<'Cd'>}` */
                  : `${ToKebab<Head>}-${Lowercase<Caps>}${ToKebab<Tail>}` /* 'aBcD' where Caps = 'B' tail starts as lowercase */
              : never
            : never
          : never
    : S /* 'abc'  */
  : never

export type StringObject = Record<string, unknown>

function test() {
  // prettier-ignore
  const t = <A, B>(a: (
    A extends B ? (
      B extends A ? null : never
    ) : never
  )) =>{ }
  t<"foo-bar", ToKebab<"FooBar">>(null)
  // @ts-expect-error
  t<"foo-3ar", ToKebab<"FooBar">>(null)
}
