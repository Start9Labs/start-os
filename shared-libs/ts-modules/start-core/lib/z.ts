import { z as zod } from 'zod'
import type { DeepPartial } from './types'

export * from 'zod'

const loose = (schema: any): any => {
  const def = schema._zod?.def
  if (!def) return schema
  switch (def.type) {
    case 'optional':
      return loose(def.innerType).optional()
    case 'nullable':
      return loose(def.innerType).nullable()
    case 'object':
      return zod.looseObject(
        Object.fromEntries(
          Object.entries(schema.shape).map(([key, value]) => [
            key,
            loose(value),
          ]),
        ),
      )
    case 'array':
      return zod.array(loose(def.element))
    case 'union':
      return zod.union(def.options.map(loose))
    case 'intersection':
      return zod.intersection(loose(def.left), loose(def.right))
    case 'record':
      return zod.record(def.keyType, loose(def.valueType))
    case 'tuple':
      return zod.tuple(def.items.map(loose))
    case 'lazy':
      return zod.lazy(() => loose(def.getter()))
    default:
      return schema
  }
}

const partial = (schema: zod.ZodType): any => {
  const def = (schema as any)._zod.def
  switch (def.type) {
    case 'optional':
      return partial(def.innerType).optional()
    case 'nullable':
      return partial(def.innerType).nullable()
    case 'default':
      return partial(def.innerType).default(def.defaultValue)
    case 'prefault':
      return partial(def.innerType).prefault(def.defaultValue)
    case 'catch':
      return partial(def.innerType).catch(def.catchValue)
    case 'nonoptional':
      return partial(def.innerType).nonoptional()
    case 'readonly':
      return partial(def.innerType).readonly()
    case 'object': {
      const shape = Object.fromEntries(
        Object.entries(def.shape).map(([k, v]) => [
          k,
          partial(v as zod.ZodType).optional(),
        ]),
      )
      const object = zod.object(shape)
      return def.catchall ? object.catchall(def.catchall) : object
    }
    case 'array':
      return zod.array(partial(def.element))
    case 'union': {
      const options = def.options.map((o: zod.ZodType) => {
        const d = (o as any)._zod.def
        if (!def.discriminator || d.type !== 'object') return partial(o)
        return partial(o).extend({
          [def.discriminator]: d.shape[def.discriminator],
        })
      })
      return def.discriminator
        ? zod.discriminatedUnion(def.discriminator, options)
        : zod.union(options)
    }
    case 'intersection':
      return zod.intersection(partial(def.left), partial(def.right))
    case 'record':
      return zod.partialRecord(def.keyType, partial(def.valueType))
    case 'tuple':
      return zod.tuple(def.items.map(deepPartial))
    case 'lazy':
      return zod.lazy(() => partial(def.getter()))
    default:
      return schema
  }
}

/** Every object at every level keeps unknown keys. */
export function deepLoose<T>(schema: zod.ZodType<T>): zod.ZodType<T> {
  return loose(schema)
}

/** Every key optional at every level; each object keeps its own unknown-key mode and a discriminated union its discriminator. */
export function deepPartial<T>(
  schema: zod.ZodType<T>,
): zod.ZodType<DeepPartial<T>> {
  return partial(schema)
}
