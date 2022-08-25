import { applyOperation } from 'fast-json-patch'
import matches, {
  Parser,
  shape,
  string,
  literal,
  boolean,
  deferred,
  dictionary,
  anyOf,
  number,
  arrayOf,
} from 'ts-matches'

type ValidVersion = 1 | 2

type PropertiesV1 = typeof matchPropertiesV1._TYPE
type PackagePropertiesV1 = PropertiesV1[]
type PackagePropertiesV2 = {
  [name: string]: PackagePropertyString | PackagePropertyObject
}
type PackagePropertiesVersionedData<T extends number> = T extends 1
  ? PackagePropertiesV1
  : T extends 2
  ? PackagePropertiesV2
  : never

type PackagePropertyString = typeof matchPackagePropertyString._TYPE

export type PackagePropertiesVersioned<T extends number> = {
  version: T
  data: PackagePropertiesVersionedData<T>
}
export type PackageProperties = PackagePropertiesV2

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
const matchPackagePropertyObject = shape(
  {
    type: literal('object'),
    value: matchPackagePropertiesV2,
    description: string,
  },
  ['description'],
)

const matchPropertyV2 = anyOf(
  matchPackagePropertyString,
  matchPackagePropertyObject,
)
type PackagePropertyObject = typeof matchPackagePropertyObject._TYPE
setPPV2(dictionary([string, matchPropertyV2]))

const matchPackagePropertiesVersionedV1 = shape({
  version: number,
  data: arrayOf(matchPropertiesV1),
})
const matchPackagePropertiesVersionedV2 = shape({
  version: number,
  data: dictionary([string, matchPropertyV2]),
})

export function parsePropertiesPermissive(
  properties: unknown,
  errorCallback: (err: Error) => any = console.warn,
): PackageProperties {
  return matches(properties)
    .when(matchPackagePropertiesVersionedV1, prop =>
      parsePropertiesV1Permissive(prop.data, errorCallback),
    )
    .when(matchPackagePropertiesVersionedV2, prop => prop.data)
    .when(matches.nill, {})
    .defaultToLazy(() => {
      errorCallback(new TypeError(`value is not valid`))
      return {}
    })
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
        const dataPath = error.keys.map(removeQuotes).join('/')
        errorCallback(new Error(`/data/${idx}: ${message}`))
        if (dataPath) {
          applyOperation(cur, {
            op: 'replace',
            path: `/${dataPath}`,
            value: undefined,
          })
        }
      }
      return prev
    },
    {},
  )
}

const removeRegex = /('|")/
function removeQuotes(x: string) {
  while (removeRegex.test(x)) {
    x = x.replace(removeRegex, '')
  }
  return x
}
