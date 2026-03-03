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

// Recursively make all ZodObjects in a schema loose (preserve extra keys at every nesting level).
// Uses _zod.def.type duck-typing instead of instanceof to avoid issues with mismatched zod versions.
function deepLoose<S extends _z.ZodType>(schema: S): S {
  const def = (schema as any)._zod?.def
  if (!def) return schema
  let result: _z.ZodType
  switch (def.type) {
    case 'optional':
      result = deepLoose(def.innerType).optional()
      break
    case 'nullable':
      result = deepLoose(def.innerType).nullable()
      break
    case 'object': {
      const newShape: Record<string, _z.ZodType> = {}
      for (const key in (schema as any).shape) {
        newShape[key] = deepLoose((schema as any).shape[key])
      }
      result = _z.looseObject(newShape)
      break
    }
    case 'array':
      result = _z.array(deepLoose(def.element))
      break
    case 'union':
      result = _z.union(def.options.map((o: _z.ZodType) => deepLoose(o)))
      break
    case 'intersection':
      result = _z.intersection(deepLoose(def.left), deepLoose(def.right))
      break
    case 'record':
      result = _z.record(def.keyType, deepLoose(def.valueType))
      break
    case 'tuple':
      result = _z.tuple(def.items.map((i: _z.ZodType) => deepLoose(i)))
      break
    case 'lazy':
      result = _z.lazy(() => deepLoose(def.getter()))
      break
    default:
      return schema
  }
  return result as S
}

type ZodDeepLoose = <T>(a: _z.ZodType<T>) => _z.ZodType<T>

// Add deepPartial and deepLoose to z at runtime
;(_z as any).deepPartial = <T>(a: _z.ZodType<T>) => deepLoose(zodDeepPartial(a))
;(_z as any).deepLoose = deepLoose

// Augment zod's z namespace so z.deepPartial and z.deepLoose are typed
declare module 'zod' {
  namespace z {
    const deepPartial: ZodDeepPartial
    const deepLoose: ZodDeepLoose
  }
}

// Override z.object to produce loose objects by default (extra keys are preserved, not stripped).
const _origObject = _z.object
const _patchedObject = (...args: Parameters<typeof _z.object>) =>
  _origObject(...args).loose()

// In CJS (Node.js), patch the source module in require.cache where 'object' is a writable property;
// the CJS getter chain (index → external → schemas) then relays the patched version.
// We walk only the zod entry module's dependency tree and match by identity (=== origObject).
try {
  const _zodModule = require.cache[require.resolve('zod')]
  for (const child of _zodModule?.children ?? []) {
    for (const grandchild of child.children ?? []) {
      const desc = Object.getOwnPropertyDescriptor(grandchild.exports, 'object')
      if (desc?.value === _origObject && desc.writable) {
        grandchild.exports.object = _patchedObject
      }
    }
  }
} catch (_) {
  // Not in CJS/Node environment (e.g. browser) — require.cache unavailable
}

// z.object is a non-configurable getter on the zod namespace, so we can't override it directly.
// Shadow it by exporting a new object with _z as prototype and our patched object on the instance.
const z: typeof _z = Object.create(_z, {
  object: {
    value: _patchedObject,
    writable: true,
    configurable: true,
    enumerable: true,
  },
})

export { z }

export * as utils from './util'
