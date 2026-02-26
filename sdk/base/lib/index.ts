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
import type { DeepPartial } from './types'

type ZodDeepPartial = <T>(a: _z.ZodType<T>) => _z.ZodType<DeepPartial<T>>

// Add deepPartial to z at runtime, wrapping with .passthrough() to allow extra keys
;(_z as any).deepPartial = <T>(a: _z.ZodType<T>) =>
  (zodDeepPartial(a) as any).passthrough()

// Augment zod's z namespace so z.deepPartial is typed
declare module 'zod' {
  namespace z {
    const deepPartial: ZodDeepPartial
  }
}

export { _z as z }

export * as utils from './util'
