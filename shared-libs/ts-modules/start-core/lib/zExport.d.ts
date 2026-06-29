import { z as _z } from 'zod'
import type { DeepPartial } from './types'

type ZodDeepPartial = <T>(a: _z.ZodType<T>) => _z.ZodType<DeepPartial<T>>
type ZodDeepLoose = <T>(a: _z.ZodType<T>) => _z.ZodType<T>

declare module 'zod' {
  namespace z {
    const deepPartial: ZodDeepPartial
    const deepLoose: ZodDeepLoose
  }
}

export { _z as z }
