import * as Ajv from 'ajv'
import { applyOperation } from 'fast-json-patch'


export type ValidVersion = 1 | 2

function has<Obj extends {}, K extends string>(obj: Obj, key: K): obj is (Obj & { [P in K]: unknown }) {
  return key in obj
}


const ajv = new Ajv({ jsonPointers: true, allErrors: true, nullable: true })
const ajvWithDefaults = new Ajv({ jsonPointers: true, allErrors: true, useDefaults: true, nullable: true, removeAdditional:true })

const schemaV1 = {
  'type': 'object',
  'properties': {
    'name': { 'type': 'string' },
    'value': { 'type': 'string' },
    'description': { 'type': 'string', 'nullable': true, 'default': null },
    'copyable': { 'type': 'boolean', 'default': false },
    'qr': { 'type': 'boolean', 'default': false },
  },
  'required': ['name', 'value', 'copyable', 'qr'],
  'additionalProperties': false,
}
const schemaV1Compiled = ajv.compile(schemaV1)
function isSchemaV1(properties: unknown): properties is PropertiesV1 {
  return schemaV1Compiled(properties) as any
}
const _schemaV1CompiledWithDefaults = ajvWithDefaults.compile(schemaV1)
function schemaV1CompiledWithDefaults(properties: unknown): properties is PropertiesV1 {
  return _schemaV1CompiledWithDefaults(properties) as any
}
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

export function parsePropertiesPermissive (properties: unknown, errorCallback: (err: Error) => any = console.warn): PackageProperties {
  if (typeof properties !== 'object' || properties === null) {
    errorCallback(new TypeError(`${properties} is not an object`))
    return {}
  }
  // @TODO still need this conditional?
  if (!has(properties, 'version') || !has(properties, 'data') || typeof properties.version !== 'number' || !properties.data) {
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
    case 1:
      return parsePropertiesV1Permissive(properties.data, errorCallback)
    case 2:
      return parsePropertiesV2Permissive(properties.data, errorCallback)
    default:
      errorCallback(new Error(`unknown properties version ${properties.version}, attempting to parse as v2`))
      return parsePropertiesV2Permissive(properties.data, errorCallback)
  }
}



function parsePropertiesV1Permissive (properties: unknown, errorCallback: (err: Error) => any): PackageProperties {
  if (!Array.isArray(properties)) {
    errorCallback(new TypeError(`${properties} is not an array`))
    return {}
  }
  const parsedProperties : PackagePropertiesV2 = {};
  for(const idx in properties) {
    const cur:unknown = properties[idx]
    if(isSchemaV1(cur)) {
      parsedProperties[cur.name] = {
        type: 'string',
        value: cur.value,
        description: cur.description,
        copyable: cur.copyable,
        qr: cur.qr,
        masked: false,
      }
    }
    else if (schemaV1Compiled.errors) {
      for (let err of schemaV1Compiled.errors) {
        errorCallback(new Error(`/data/${idx}${err.dataPath}: ${err.message}`))
        if (err.dataPath) {
          applyOperation(cur, { op: 'replace', path: err.dataPath, value: undefined })
        }
      }
      if (!schemaV1CompiledWithDefaults(cur)) {
        for (let err of _schemaV1CompiledWithDefaults.errors) {
          errorCallback(new Error(`/data/${idx}${err.dataPath}: ${err.message}`))
        }
        continue
      }
      parsedProperties[cur.name] = {

        type: 'string',
        value: cur.value,
        description: cur.description,
        copyable: cur.copyable,
        qr: cur.qr,
        masked: false,
      }
    }
    
  }
  return parsedProperties
}
function parsePropertiesV2Permissive (properties: unknown, errorCallback: (err: Error) => any): PackageProperties {
  if (typeof properties !== 'object' || properties === null) {
    errorCallback(new TypeError(`${properties} is not an object`))
    return {}
  }
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
interface PropertiesV1 {
  name: string
  value: string
  description: string | null
  copyable: boolean
  qr: boolean
}

type PackagePropertiesV1 = PropertiesV1[]
export type PackageProperties = PackagePropertiesV2

export type PackagePropertiesVersioned<T extends number> = {
  version: T,
  data: PackagePropertiesVersionedData<T>
}

export type PackagePropertiesVersionedData<T extends number> =
  T extends 1 ? PackagePropertiesV1 :
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

