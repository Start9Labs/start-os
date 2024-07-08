import * as T from "../types"

export { GetServiceInterface, getServiceInterface } from "./getServiceInterface"
export { getServiceInterfaces } from "./getServiceInterfaces"
export { addressHostToUrl } from "./getServiceInterface"
export { getDefaultString } from "./getDefaultString"
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
