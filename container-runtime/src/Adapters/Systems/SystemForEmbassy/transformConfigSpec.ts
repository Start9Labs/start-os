import { CT } from "@start9labs/start-sdk"

export function transformConfigSpec(oldSpec: OldConfigSpec): CT.InputSpec {
  return Object.entries(oldSpec).reduce((inputSpec, [key, oldVal]) => {
    let newVal: CT.ValueSpec

    if (oldVal.type === "boolean") {
      newVal = {
        type: "toggle",
        name: oldVal.name,
        default: oldVal.default,
        description: oldVal.description || null,
        warning: oldVal.warning || null,
        disabled: false,
        immutable: false,
      }
    } else if (oldVal.type === "enum") {
      newVal = {
        type: "select",
        name: oldVal.name,
        description: oldVal.description || null,
        warning: oldVal.warning || null,
        default: oldVal.default,
        values: oldVal.values.reduce(
          (obj, curr) => ({
            ...obj,
            [curr]: oldVal["value-names"][curr],
          }),
          {},
        ),
        required: false,
        disabled: false,
        immutable: false,
      }
    } else if (oldVal.type === "list") {
      newVal = getListSpec(oldVal)
    } else if (oldVal.type === "number") {
      const range = Range.from(oldVal.range)

      newVal = {
        type: "number",
        name: oldVal.name,
        default: oldVal.default || null,
        description: oldVal.description || null,
        warning: oldVal.warning || null,
        disabled: false,
        immutable: false,
        required: !oldVal.nullable,
        min: range.min
          ? range.minInclusive
            ? range.min
            : range.min + 1
          : null,
        max: range.max
          ? range.maxInclusive
            ? range.max
            : range.max - 1
          : null,
        integer: oldVal.integral,
        step: null,
        units: oldVal.units || null,
        placeholder: oldVal.placeholder || null,
      }
    } else if (oldVal.type === "object") {
      newVal = {
        type: "object",
        name: oldVal.name,
        description: oldVal.description || null,
        warning: oldVal.warning || null,
        spec: transformConfigSpec(oldVal.spec),
      }
    } else if (oldVal.type === "string") {
      newVal = {
        type: "text",
        name: oldVal.name,
        default: oldVal.default || null,
        description: oldVal.description || null,
        warning: oldVal.warning || null,
        disabled: false,
        immutable: false,
        required: !oldVal.nullable,
        patterns:
          oldVal.pattern && oldVal["pattern-description"]
            ? [
                {
                  regex: oldVal.pattern,
                  description: oldVal["pattern-description"],
                },
              ]
            : [],
        minLength: null,
        maxLength: null,
        masked: oldVal.masked,
        generate: null,
        inputmode: "text",
        placeholder: oldVal.placeholder || null,
      }
    } else {
      newVal = {
        type: "union",
        name: oldVal.tag.name,
        description: oldVal.tag.description || null,
        warning: oldVal.tag.warning || null,
        variants: Object.entries(oldVal.variants).reduce(
          (obj, [id, spec]) => ({
            ...obj,
            [id]: {
              name: oldVal.tag["variant-names"][id],
              spec: transformConfigSpec(spec),
            },
          }),
          {} as Record<string, { name: string; spec: CT.InputSpec }>,
        ),
        disabled: false,
        required: true,
        default: oldVal.default,
        immutable: false,
      }
    }

    return {
      ...inputSpec,
      [key]: newVal,
    }
  }, {} as CT.InputSpec)
}

function getListSpec(
  oldVal: OldValueSpecList,
): CT.ValueSpecMultiselect | CT.ValueSpecList {
  const range = Range.from(oldVal.range)

  let partial: Omit<CT.ValueSpecList, "type" | "spec" | "default"> = {
    name: oldVal.name,
    description: oldVal.description || null,
    warning: oldVal.warning || null,
    minLength: range.min
      ? range.minInclusive
        ? range.min
        : range.min + 1
      : null,
    maxLength: range.max
      ? range.maxInclusive
        ? range.max
        : range.max - 1
      : null,
    disabled: false,
  }

  if (isEnumList(oldVal)) {
    return {
      ...partial,
      type: "multiselect",
      default: oldVal.default as string[],
      immutable: false,
      values: oldVal.spec.values.reduce(
        (obj, curr) => ({
          ...obj,
          [curr]: oldVal.spec["value-names"][curr],
        }),
        {},
      ),
    }
  } else if (isStringList(oldVal)) {
    return {
      ...partial,
      type: "list",
      default: oldVal.default as string[],
      spec: {
        type: "text",
        patterns:
          oldVal.spec.pattern && oldVal.spec["pattern-description"]
            ? [
                {
                  regex: oldVal.spec.pattern,
                  description: oldVal.spec["pattern-description"],
                },
              ]
            : [],
        minLength: null,
        maxLength: null,
        masked: oldVal.spec.masked,
        generate: null,
        inputmode: "text",
        placeholder: oldVal.spec.placeholder || null,
      },
    }
  } else if (isObjectList(oldVal)) {
    return {
      ...partial,
      type: "list",
      default: oldVal.default as Record<string, unknown>[],
      spec: {
        type: "object",
        spec: transformConfigSpec(oldVal.spec.spec),
        uniqueBy: oldVal.spec["unique-by"],
        displayAs: oldVal.spec["display-as"] || null,
      },
    }
  } else {
    throw new Error("Invalid list subtype. enum, string, and object permitted.")
  }
}

function isEnumList(val: OldValueSpecList): val is OldValueSpecListOf<"enum"> {
  return val.subtype === "enum"
}

function isStringList(
  val: OldValueSpecList,
): val is OldValueSpecListOf<"string"> {
  return val.subtype === "string"
}

function isObjectList(
  val: OldValueSpecList,
): val is OldValueSpecListOf<"object"> {
  return val.subtype === "object"
}

type OldConfigSpec = Record<string, OldValueSpec>

type OldValueType =
  | "string"
  | "number"
  | "boolean"
  | "enum"
  | "list"
  | "object"
  | "pointer"
  | "union"
type OldValueSpec = OldValueSpecOf<OldValueType>

// core spec types. These types provide the metadata for performing validations
type OldValueSpecOf<T extends OldValueType> = T extends "string"
  ? OldValueSpecString
  : T extends "number"
    ? OldValueSpecNumber
    : T extends "boolean"
      ? OldValueSpecBoolean
      : T extends "enum"
        ? OldValueSpecEnum
        : T extends "list"
          ? OldValueSpecList
          : T extends "object"
            ? OldValueSpecObject
            : T extends "union"
              ? OldValueSpecUnion
              : never

interface OldValueSpecString extends OldListValueSpecString {
  type: "string"
  default?: OldDefaultString
  nullable: boolean
  textarea?: boolean
  name: string
  description?: string
  warning?: string
}

interface OldValueSpecNumber {
  type: "number"
  nullable: boolean
  default?: number
  name: string
  description?: string
  warning?: string
  range: string
  integral: boolean
  units?: string
  placeholder?: string
}

interface OldValueSpecEnum extends OldListValueSpecEnum {
  type: "enum"
  default: string
  name: string
  description?: string
  warning?: string
}

interface OldValueSpecBoolean {
  type: "boolean"
  default: boolean
  name: string
  description?: string
  warning?: string
}

interface OldValueSpecUnion {
  type: "union"
  tag: OldUnionTagSpec
  variants: { [key: string]: OldConfigSpec }
  default: string
}

interface OldValueSpecObject {
  type: "object"
  spec: OldConfigSpec
  name: string
  description?: string
  warning?: string
}

// no lists of booleans, lists, pointers
type OldListValueSpecType = "string" | "enum" | "object"

// represents a spec for the values of a list
type OldListValueSpecOf<T extends OldListValueSpecType> = T extends "string"
  ? OldListValueSpecString
  : T extends "enum"
    ? OldListValueSpecEnum
    : T extends "object"
      ? OldListValueSpecObject
      : never

// represents a spec for a list
type OldValueSpecList = OldValueSpecListOf<OldListValueSpecType>
interface OldValueSpecListOf<T extends OldListValueSpecType> {
  type: "list"
  subtype: T
  spec: OldListValueSpecOf<T>
  range: string // '[0,1]' (inclusive) OR '[0,*)' (right unbounded), normal math rules
  default: string[] | number[] | OldDefaultString[] | object[]
  name: string
  description?: string
  warning?: string
}

interface OldListValueSpecString {
  pattern?: string
  "pattern-description"?: string
  masked: boolean
  copyable: boolean
  placeholder?: string
}

interface OldListValueSpecEnum {
  values: string[]
  "value-names": { [value: string]: string }
}

interface OldListValueSpecObject {
  spec: OldConfigSpec // this is a mapped type of the config object at this level, replacing the object's values with specs on those values
  "unique-by": OldUniqueBy // indicates whether duplicates can be permitted in the list
  "display-as"?: string // this should be a handlebars template which can make use of the entire config which corresponds to 'spec'
}

type OldUniqueBy =
  | null
  | string
  | { any: OldUniqueBy[] }
  | { all: OldUniqueBy[] }

interface OldUnionTagSpec {
  id: string // The name of the field containing one of the union variants
  "variant-names": {
    // the name of each variant
    [variant: string]: string
  }
  name: string
  description?: string
  warning?: string
}

type OldDefaultString = string | { charset: string; len: number }

export class Range {
  min?: number
  max?: number
  minInclusive!: boolean
  maxInclusive!: boolean

  static from(s: string = "(*,*)"): Range {
    const r = new Range()
    r.minInclusive = s.startsWith("[")
    r.maxInclusive = s.endsWith("]")
    const [minStr, maxStr] = s.split(",").map((a) => a.trim())
    r.min = minStr === "(*" ? undefined : Number(minStr.slice(1))
    r.max = maxStr === "*)" ? undefined : Number(maxStr.slice(0, -1))
    return r
  }
}
