import * as Ajv from 'ajv'
import { JsonPointer } from 'jsonpointerx'

const ajv = new Ajv({ jsonPointers: true, allErrors: true, nullable: true })
const ajvWithDefaults = new Ajv({ jsonPointers: true, allErrors: true, useDefaults: true, nullable: true, removeAdditional: 'failing' })
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
const schemaV1CompiledWithDefaults = ajvWithDefaults.compile(schemaV1)
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

export function parseMetricsPermissive (metrics: any, errorCallback: (err: Error) => any = console.warn): AppMetrics {
  if (typeof metrics !== 'object' || metrics === null) {
    errorCallback(new TypeError(`${metrics} is not an object`))
    return { }
  }
  if (typeof metrics.version !== 'number' || !metrics.data) {
    return Object.entries(metrics)
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
  const typedMetrics = metrics as AppMetricsVersioned<number>
  switch (typedMetrics.version) {
    case 1:
      return parseMetricsV1Permissive(typedMetrics.data, errorCallback)
    case 2:
      return parseMetricsV2Permissive(typedMetrics.data, errorCallback)
    default:
      errorCallback(new Error(`unknown metrics version ${metrics.version}, attempting to parse as v2`))
      return parseMetricsV2Permissive(typedMetrics.data, errorCallback)
  }
}

function parseMetricsV1Permissive (metrics: AppMetricsV1, errorCallback: (err: Error) => any): AppMetrics {
  return metrics.reduce((prev: AppMetricsV2, cur: AppMetricV1, idx: number) => {
    schemaV1Compiled(cur)
    if (schemaV1Compiled.errors) {
      for (let err of schemaV1Compiled.errors) {
        errorCallback(new Error(`/data/${idx}${err.dataPath}: ${err.message}`))
        if (err.dataPath) {
          JsonPointer.set(cur, err.dataPath, undefined)
        }
      }
      if (!schemaV1CompiledWithDefaults(cur)) {
        for (let err of schemaV1CompiledWithDefaults.errors) {
          errorCallback(new Error(`/data/${idx}${err.dataPath}: ${err.message}`))
        }
        return prev
      }
    }
    prev[cur.name] = {
      type: 'string',
      value: cur.value,
      description: cur.description,
      copyable: cur.copyable,
      qr: cur.qr,
      masked: false,
    }
    return prev
  }, { })
}

function parseMetricsV2Permissive (metrics: AppMetricsV2, errorCallback: (err: Error) => any): AppMetrics {
  return Object.entries(metrics).reduce((prev, [name, value], idx) => {
    schemaV2Compiled(value)
    if (schemaV2Compiled.errors) {
      for (let err of schemaV2Compiled.errors) {
        errorCallback(new Error(`/data/${idx}${err.dataPath}: ${err.message}`))
        if (err.dataPath) {
          JsonPointer.set(value, err.dataPath, undefined)
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

export type AppMetrics = AppMetricsV2 // chnage this type when updating versions

export type AppMetricsVersioned<T extends number> = {
  version: T,
  data: AppMetricsVersionedData<T>
}

export type AppMetricsVersionedData<T extends number> = T extends 1 ? AppMetricsV1 :
  T extends 2 ? AppMetricsV2 :
  never

interface AppMetricV1 {
  name: string
  value: string
  description: string | null
  copyable: boolean
  qr: boolean
}

type AppMetricsV1 = AppMetricV1[]

interface AppMetricsV2 {
  [name: string]: AppMetricString | AppMetricObject
}

interface AppMetricBase {
  type: 'string' | 'object'
  description: string | null
}

interface AppMetricString extends AppMetricBase {
  type: 'string'
  value: string
  copyable: boolean
  qr: boolean
  masked: boolean
}

interface AppMetricObject extends AppMetricBase {
  type: 'object'
  value: AppMetricsV2
}