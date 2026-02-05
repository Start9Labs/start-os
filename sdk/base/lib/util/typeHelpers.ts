/**
 * @module typeHelpers
 *
 * Utility types and type guards used throughout the SDK for type manipulation
 * and runtime type checking.
 */
import * as T from "../types"

/**
 * Flattens an intersection type into a single object type.
 * Makes hover information more readable by expanding intersections.
 *
 * @example
 * ```typescript
 * type A = { foo: string }
 * type B = { bar: number }
 * type AB = A & B  // Shows as "A & B"
 * type Flat = FlattenIntersection<A & B>  // Shows as { foo: string; bar: number }
 * ```
 */
// prettier-ignore
export type FlattenIntersection<T> =
T extends ArrayLike<any> ? T :
T extends object ? {} & {[P in keyof T]: T[P]} :
 T;

/**
 * Alias for FlattenIntersection for shorter usage.
 * @see FlattenIntersection
 */
export type _<T> = FlattenIntersection<T>

/**
 * Type guard to check if a value is a KnownError.
 * KnownError is the standard error format for service operations.
 *
 * @param e - The value to check
 * @returns True if the value is a KnownError object
 */
export const isKnownError = (e: unknown): e is T.KnownError =>
  e instanceof Object && ("error" in e || "error-code" in e)

/** @internal Symbol for affine type branding */
declare const affine: unique symbol

/**
 * Type brand for creating affine types (types that can only be used in specific contexts).
 * Used to prevent values from being used outside their intended context.
 *
 * @typeParam A - The context identifier
 *
 * @example
 * ```typescript
 * type BackupEffects = Effects & Affine<"Backups">
 * // BackupEffects can only be created in backup context
 * ```
 */
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
