import * as T from "../types"

import "./nullIfEmpty"
import "./fileHelper"
import "../store/getStore"
import "./deepEqual"
import "./deepMerge"
import "./Overlay"
import "./once"
import * as utils from "./utils"
import { SDKManifest } from "../manifest/ManifestTypes"

// prettier-ignore
export type FlattenIntersection<T> = 
T extends ArrayLike<any> ? T :
T extends object ? {} & {[P in keyof T]: T[P]} :
 T;

export type _<T> = FlattenIntersection<T>

export const isKnownError = (e: unknown): e is T.KnownError =>
  e instanceof Object && ("error" in e || "error-code" in e)

declare const affine: unique symbol

export const createUtils = utils.createUtils
export const createMainUtils = <Manifest extends SDKManifest, Store>(
  effects: T.Effects,
) => createUtils<Manifest, Store, {}>(effects)

type NeverPossible = { [affine]: string }
export type NoAny<A> = NeverPossible extends A
  ? keyof NeverPossible extends keyof A
    ? never
    : A
  : A
