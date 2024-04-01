// prettier-ignore
export type ReadonlyDeep<A> = 
    A extends Function ? A :
    A extends {} ? { readonly [K in keyof A]: ReadonlyDeep<A[K]> } : A;
export type MaybePromise<A> = Promise<A> | A
export type Message = string

import "./DependencyConfig"
import "./setupDependencyConfig"
