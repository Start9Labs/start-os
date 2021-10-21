import * as Ajv from 'ajv'
import { applyOperation } from 'fast-json-patch'

const ajv = new Ajv({ jsonPointers: true, allErrors: true, nullable: true })
const ajvWithDefaults = new Ajv({ jsonPointers: true, allErrors: true, useDefaults: true, nullable: true, removeAdditional: 'failing' })
const schemaV2 = {
  'anyOf': [
    {
      'type': 'object',
      'properties': {
        'type': { 'type': 'string', 'const': 'string' },
        'value': { 'type': 'string' },
        'description': { 'type': 'string', 'nullable': true, 'default': null },
        'copyable': { 'type': 'boolean', 'default': false },
        'qr': { 'type': 'boolean', 'default': false },
        'masked': { 'type': 'boolean', 'default': false },
      },
      'required': ['type', 'value', 'description', 'copyable', 'qr', 'masked'],
      'additionalProperties': false,
    },
    {
      'type': 'object',
      'properties': {
        'type': { 'type': 'string', 'const': 'object' },
        'value': {
          'type': 'object',
          'patternProperties': {
            '^.*$': {
              '$ref': '#',
            },
          },
        },
        'description': { 'type': 'string', 'nullable': true, 'default': null },

      },
      'required': ['type', 'value', 'description'],
      'additionalProperties': false,
    },
  ],
}
const schemaV2Compiled = ajv.compile(schemaV2)
const schemaV2CompiledWithDefaults = ajvWithDefaults.compile(schemaV2)

export function parsePropertiesPermissive (properties: any, errorCallback: (err: Error) => any = console.warn): PackageProperties {
  if (typeof properties !== 'object' || properties === null) {
    errorCallback(new TypeError(`${properties} is not an object`))
    return { }
  }
  // @TODO still need this conditional?
  if (typeof properties.version !== 'number' || !properties.data) {
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
          description: null,
          copyable: false,
          qr: false,
          masked: false,
        },
      }))
      .reduce((acc, { name, value }) => {
        acc[name] = value
        return acc
      }, { })
  }
  switch (properties.version) {
    case 2:
      return parsePropertiesV2Permissive(properties.data, errorCallback)
    default:
      errorCallback(new Error(`unknown properties version ${properties.version}, attempting to parse as v2`))
      return parsePropertiesV2Permissive(properties.data, errorCallback)
  }
}

function parsePropertiesV2Permissive (properties: PackagePropertiesV2, errorCallback: (err: Error) => any): PackageProperties {
  return Object.entries(properties).reduce((prev, [name, value], idx) => {
    schemaV2Compiled(value)
    if (schemaV2Compiled.errors) {
      for (let err of schemaV2Compiled.errors) {
        errorCallback(new Error(`/data/${idx}${err.dataPath}: ${err.message}`))
        if (err.dataPath) {
          applyOperation(value, { op: 'replace', path: err.dataPath, value: undefined })
        }
      }
      if (!schemaV2CompiledWithDefaults(value)) {
        for (let err of schemaV2CompiledWithDefaults.errors) {
          errorCallback(new Error(`/data/${idx}${err.dataPath}: ${err.message}`))
        }
        return prev
      }
    }
    prev[name] = value
    return prev
  }, { })
}

export type PackageProperties = PackagePropertiesV2 // change this type when updating versions

export type PackagePropertiesVersioned<T extends number> = {
  version: T,
  data: PackagePropertiesVersionedData<T>
}

export type PackagePropertiesVersionedData<T extends number> =
  T extends 2 ? PackagePropertiesV2 :
  never

interface PackagePropertiesV2 {
  [name: string]: PackagePropertyString | PackagePropertyObject
}

interface PackagePropertyBase {
  type: 'string' | 'object'
  description: string | null
}

interface PackagePropertyString extends PackagePropertyBase {
  type: 'string'
  value: string
  copyable: boolean
  qr: boolean
  masked: boolean
}

interface PackagePropertyObject extends PackagePropertyBase {
  type: 'object'
  value: PackagePropertiesV2
}