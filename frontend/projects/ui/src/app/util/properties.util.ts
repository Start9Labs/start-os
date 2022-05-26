import { applyOperation } from 'fast-json-patch'
import {
  Parser,
  shape,
  string,
  literal,
  boolean,
  object,
  deferred,
  dictionary,
  anyOf,
} from 'ts-matches'

export type ValidVersion = 1 | 2

function has<Obj extends {}, K extends string>(
  obj: Obj,
  key: K,
): obj is Obj & { [P in K]: unknown } {
  return key in obj
}

const matchPropertiesV1 = shape(
  {
    name: string,
    value: string,
    description: string,
    copyable: boolean,
    qr: boolean,
  },
  ['description', 'copyable', 'qr'],
  { copyable: false, qr: false } as const,
)
type PropertiesV1 = typeof matchPropertiesV1._TYPE

type PackagePropertiesV2 = {
  [name: string]: PackagePropertyString | PackagePropertyObject
}

const [matchPackagePropertiesV2, setPPV2] = deferred<PackagePropertiesV2>()
const matchPackagePropertyString = shape(
  {
    type: literal('string'),
    description: string,
    value: string,
    copyable: boolean,
    qr: boolean,
    masked: boolean,
  },
  ['description', 'copyable', 'qr', 'masked'],
  {
    copyable: false,
    qr: false,
    masked: false,
  } as const,
)
type PackagePropertyString = typeof matchPackagePropertyString._TYPE
const matchPackagePropertyObject = shape(
  {
    type: literal('object'),
    value: matchPackagePropertiesV2,
    description: string.optional(),
  },
  ['description'],
  { description: null as null },
)
const matchPropertyV2 = anyOf(
  matchPackagePropertyString,
  matchPackagePropertyObject,
)
type PackagePropertyObject = typeof matchPackagePropertyObject._TYPE
setPPV2(dictionary([string, matchPropertyV2]))

export function parsePropertiesPermissive(
  properties: unknown,
  errorCallback: (err: Error) => any = console.warn,
): PackageProperties {
  if (typeof properties !== 'object' || properties === null) {
    errorCallback(new TypeError(`${properties} is not an object`))
    return {}
  }
  // @TODO still need this conditional?
  if (
    !has(properties, 'version') ||
    !has(properties, 'data') ||
    typeof properties.version !== 'number' ||
    !properties.data
  ) {
    return Object.entries(properties)
      .filter(([_, value]) => {
        if (typeof value === 'string') {
          return true
        } else {
          errorCallback(new TypeError(`${value} is not a string`))
          return false
        }
      })
      .map(([name, value]) => ({
        name,
        value: {
          value: String(value),
          copyable: false,
          qr: false,
          masked: false,
        },
      }))
      .reduce((acc, { name, value }) => {
        // TODO: Fix type
        acc[name] = value as any
        return acc
      }, {} as PackageProperties)
  }
  switch (properties.version) {
    case 1:
      return parsePropertiesV1Permissive(properties.data, errorCallback)
    case 2:
      return parsePropertiesV2Permissive(properties.data, errorCallback)
    default:
      errorCallback(
        new Error(
          `unknown properties version ${properties.version}, attempting to parse as v2`,
        ),
      )
      return parsePropertiesV2Permissive(properties.data, errorCallback)
  }
}

function parsePropertiesV1Permissive(
  properties: unknown,
  errorCallback: (err: Error) => any,
): PackageProperties {
  if (!Array.isArray(properties)) {
    errorCallback(new TypeError(`${properties} is not an array`))
    return {}
  }
  return properties.reduce(
    (prev: PackagePropertiesV2, cur: unknown, idx: number) => {
      const result = matchPropertiesV1.enumParsed(cur)
      if ('value' in result) {
        const value = result.value
        prev[value.name] = {
          type: 'string',
          value: value.value,
          description: value.description,
          copyable: value.copyable,
          qr: value.qr,
          masked: false,
        }
      } else {
        const error = result.error
        const message = Parser.validatorErrorAsString(error)
        let dataPath = error.keys.map(x => JSON.parse(x)).join('/')
        errorCallback(new Error(`/data/${idx}: ${message}`))
        if (dataPath) {
          applyOperation(cur, {
            op: 'replace',
            path: dataPath,
            value: undefined,
          })
        }
      }
      return prev
    },
    {},
  )
}
function parsePropertiesV2Permissive(
  properties: unknown,
  errorCallback: (err: Error) => any,
): PackageProperties {
  if (!object.test(properties)) {
    return {}
  }
  return Object.entries(properties).reduce(
    (prev: PackageProperties, [name, value], idx) => {
      const result = matchPropertyV2.enumParsed(value)
      if ('value' in result) {
        prev[name] = result.value
      } else {
        const error = result.error
        const message = Parser.validatorErrorAsString(error)
        let dataPath = error.keys.map(x => JSON.parse(x)).join('/')
        errorCallback(new Error(`/data/${idx}: ${message}`))
        if (dataPath) {
          applyOperation(properties, {
            op: 'replace',
            path: dataPath,
            value: undefined,
          })
        }
      }
      return prev
    },

    {},
  )
}

type PackagePropertiesV1 = PropertiesV1[]
export type PackageProperties = PackagePropertiesV2

export type PackagePropertiesVersioned<T extends number> = {
  version: T
  data: PackagePropertiesVersionedData<T>
}

export type PackagePropertiesVersionedData<T extends number> = T extends 1
  ? PackagePropertiesV1
  : T extends 2
  ? PackagePropertiesV2
  : never
