export { S9pk } from './s9pk'
export { VersionRange, ExtendedVersion, Version } from './exver'

export * as inputSpec from './actions/input'
export * as ISB from './actions/input/builder'
export * as IST from './actions/input/inputSpecTypes'
export * as types from './types'
export * as T from './types'
export * as yaml from 'yaml'
export * as inits from './inits'
import { z as _z } from 'zod'
import { zodDeepPartial } from 'zod-deep-partial'
import { DeepPartial } from './types'

type ZodDeepPartial = <T>(a: _z.ZodType<T>) => _z.ZodType<DeepPartial<T>>

export const z: typeof _z & {
  deepPartial: ZodDeepPartial
} = Object.assign(_z, { deepPartial: zodDeepPartial as ZodDeepPartial })
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace z {
  export type infer<T extends { _zod: { output: any } }> = T['_zod']['output']
  export type input<T extends { _zod: { input: any } }> = T['_zod']['input']
  export type output<T extends { _zod: { output: any } }> = T['_zod']['output']
}

export * as utils from './util'
