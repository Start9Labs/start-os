import { IST } from "@start9labs/start-sdk"
import {
  dictionary,
  object,
  anyOf,
  string,
  literals,
  array,
  number,
  boolean,
  Parser,
  deferred,
  every,
  nill,
  literal,
} from "ts-matches"

export function transformConfigSpec(oldSpec: OldConfigSpec): IST.InputSpec {
  return Object.entries(oldSpec).reduce((inputSpec, [key, oldVal]) => {
    let newVal: IST.ValueSpec

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
            [curr]: oldVal["value-names"][curr] || curr,
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
        placeholder: oldVal.placeholder ? String(oldVal.placeholder) : null,
      }
    } else if (oldVal.type === "object") {
      newVal = {
        type: "object",
        name: oldVal.name,
        description: oldVal.description || null,
        warning: oldVal.warning || null,
        spec: transformConfigSpec(matchOldConfigSpec.unsafeCast(oldVal.spec)),
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
        masked: oldVal.masked || false,
        generate: null,
        inputmode: "text",
        placeholder: oldVal.placeholder || null,
      }
    } else if (oldVal.type === "union") {
      newVal = {
        type: "union",
        name: oldVal.tag.name,
        description: oldVal.tag.description || null,
        warning: oldVal.tag.warning || null,
        variants: Object.entries(oldVal.variants).reduce(
          (obj, [id, spec]) => ({
            ...obj,
            [id]: {
              name: oldVal.tag["variant-names"][id] || id,
              spec: transformConfigSpec(matchOldConfigSpec.unsafeCast(spec)),
            },
          }),
          {} as Record<string, { name: string; spec: IST.InputSpec }>,
        ),
        disabled: false,
        required: true,
        default: oldVal.default,
        immutable: false,
      }
    } else if (oldVal.type === "pointer") {
      return inputSpec
    } else {
      throw new Error(`unknown spec ${JSON.stringify(oldVal)}`)
    }

    return {
      ...inputSpec,
      [key]: newVal,
    }
  }, {} as IST.InputSpec)
}

export function transformOldConfigToNew(
  spec: OldConfigSpec,
  config: Record<string, any>,
): Record<string, any> {
  return Object.entries(spec).reduce((obj, [key, val]) => {
    let newVal = config[key]

    if (isObject(val)) {
      newVal = transformOldConfigToNew(
        matchOldConfigSpec.unsafeCast(val.spec),
        config[key],
      )
    }

    if (isUnion(val)) {
      const selection = config[key][val.tag.id]
      delete config[key][val.tag.id]

      newVal = {
        selection,
        value: transformOldConfigToNew(
          matchOldConfigSpec.unsafeCast(val.variants[selection]),
          config[key],
        ),
      }
    }

    if (isList(val) && isObjectList(val)) {
      newVal = (config[key] as object[]).map((obj) =>
        transformOldConfigToNew(
          matchOldConfigSpec.unsafeCast(val.spec.spec),
          obj,
        ),
      )
    }

    if (isPointer(val)) {
      return obj
    }

    return {
      ...obj,
      [key]: newVal,
    }
  }, {})
}

export function transformNewConfigToOld(
  spec: OldConfigSpec,
  config: Record<string, any>,
): Record<string, any> {
  return Object.entries(spec).reduce((obj, [key, val]) => {
    let newVal = config[key]

    if (isObject(val)) {
      newVal = transformNewConfigToOld(
        matchOldConfigSpec.unsafeCast(val.spec),
        config[key],
      )
    }

    if (isUnion(val)) {
      newVal = {
        [val.tag.id]: config[key].selection,
        ...transformNewConfigToOld(
          matchOldConfigSpec.unsafeCast(val.variants[config[key].selection]),
          config[key].value,
        ),
      }
    }

    if (isList(val) && isObjectList(val)) {
      newVal = (config[key] as object[]).map((obj) =>
        transformNewConfigToOld(
          matchOldConfigSpec.unsafeCast(val.spec.spec),
          obj,
        ),
      )
    }

    return {
      ...obj,
      [key]: newVal,
    }
  }, {})
}

function getListSpec(
  oldVal: OldValueSpecList,
): IST.ValueSpecMultiselect | IST.ValueSpecList {
  const range = Range.from(oldVal.range)

  let partial: Omit<IST.ValueSpecList, "type" | "spec" | "default"> = {
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
  } else if (isNumberList(oldVal)) {
    return {
      ...partial,
      type: "list",
      default: oldVal.default.map(String) as string[],
      spec: {
        type: "text",
        patterns: oldVal.spec.integral
          ? [{ regex: "[0-9]+", description: "Integral number type" }]
          : [
              {
                regex: "[-+]?[0-9]*\\.?[0-9]+",
                description: "Number type",
              },
            ],
        minLength: null,
        maxLength: null,
        masked: false,
        generate: null,
        inputmode: "text",
        placeholder: oldVal.spec.placeholder
          ? String(oldVal.spec.placeholder)
          : null,
      },
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
        masked: oldVal.spec.masked || false,
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
        spec: transformConfigSpec(
          matchOldConfigSpec.unsafeCast(oldVal.spec.spec),
        ),
        uniqueBy: oldVal.spec["unique-by"] || null,
        displayAs: oldVal.spec["display-as"] || null,
      },
    }
  } else {
    throw new Error("Invalid list subtype. enum, string, and object permitted.")
  }
}

function isObject(val: OldValueSpec): val is OldValueSpecObject {
  return val.type === "object"
}

function isUnion(val: OldValueSpec): val is OldValueSpecUnion {
  return val.type === "union"
}

function isList(val: OldValueSpec): val is OldValueSpecList {
  return val.type === "list"
}

function isPointer(val: OldValueSpec): val is OldValueSpecPointer {
  return val.type === "pointer"
}

function isEnumList(
  val: OldValueSpecList,
): val is OldValueSpecList & { subtype: "enum" } {
  return val.subtype === "enum"
}

function isStringList(
  val: OldValueSpecList,
): val is OldValueSpecList & { subtype: "string" } {
  return val.subtype === "string"
}
function isNumberList(
  val: OldValueSpecList,
): val is OldValueSpecList & { subtype: "number" } {
  return val.subtype === "number"
}

function isObjectList(
  val: OldValueSpecList,
): val is OldValueSpecList & { subtype: "object" } {
  if (["union"].includes(val.subtype)) {
    throw new Error("Invalid list subtype. enum, string, and object permitted.")
  }
  return val.subtype === "object"
}
export type OldConfigSpec = Record<string, OldValueSpec>
const [_matchOldConfigSpec, setMatchOldConfigSpec] = deferred<unknown>()
export const matchOldConfigSpec = _matchOldConfigSpec as Parser<
  unknown,
  OldConfigSpec
>
export const matchOldDefaultString = anyOf(
  string,
  object({ charset: string, len: number }),
)
type OldDefaultString = typeof matchOldDefaultString._TYPE

export const matchOldValueSpecString = object(
  {
    type: literals("string"),
    name: string,
    masked: boolean,
    copyable: boolean,
    nullable: boolean,
    placeholder: string,
    pattern: string,
    "pattern-description": string,
    default: matchOldDefaultString,
    textarea: boolean,
    description: string,
    warning: string,
  },
  [
    "masked",
    "copyable",
    "nullable",
    "placeholder",
    "pattern",
    "pattern-description",
    "default",
    "textarea",
    "description",
    "warning",
  ],
)

export const matchOldValueSpecNumber = object(
  {
    type: literals("number"),
    nullable: boolean,
    name: string,
    range: string,
    integral: boolean,
    default: number,
    description: string,
    warning: string,
    units: string,
    placeholder: anyOf(number, string),
  },
  ["default", "description", "warning", "units", "placeholder"],
)
type OldValueSpecNumber = typeof matchOldValueSpecNumber._TYPE

export const matchOldValueSpecBoolean = object(
  {
    type: literals("boolean"),
    default: boolean,
    name: string,
    description: string,
    warning: string,
  },
  ["description", "warning"],
)
type OldValueSpecBoolean = typeof matchOldValueSpecBoolean._TYPE

const matchOldValueSpecObject = object(
  {
    type: literals("object"),
    spec: _matchOldConfigSpec,
    name: string,
    description: string,
    warning: string,
  },
  ["description", "warning"],
)
type OldValueSpecObject = typeof matchOldValueSpecObject._TYPE

const matchOldValueSpecEnum = object(
  {
    values: array(string),
    "value-names": dictionary([string, string]),
    type: literals("enum"),
    default: string,
    name: string,
    description: string,
    warning: string,
  },
  ["description", "warning"],
)
type OldValueSpecEnum = typeof matchOldValueSpecEnum._TYPE

const matchOldUnionTagSpec = object(
  {
    id: string, // The name of the field containing one of the union variants
    "variant-names": dictionary([string, string]), // The name of each variant
    name: string,
    description: string,
    warning: string,
  },
  ["description", "warning"],
)
const matchOldValueSpecUnion = object({
  type: literals("union"),
  tag: matchOldUnionTagSpec,
  variants: dictionary([string, _matchOldConfigSpec]),
  default: string,
})
type OldValueSpecUnion = typeof matchOldValueSpecUnion._TYPE

const [matchOldUniqueBy, setOldUniqueBy] = deferred<OldUniqueBy>()
type OldUniqueBy =
  | null
  | string
  | { any: OldUniqueBy[] }
  | { all: OldUniqueBy[] }

setOldUniqueBy(
  anyOf(
    nill,
    string,
    object({ any: array(matchOldUniqueBy) }),
    object({ all: array(matchOldUniqueBy) }),
  ),
)

const matchOldListValueSpecObject = object(
  {
    spec: _matchOldConfigSpec, // this is a mapped type of the config object at this level, replacing the object's values with specs on those values
    "unique-by": matchOldUniqueBy, // indicates whether duplicates can be permitted in the list
    "display-as": string, // this should be a handlebars template which can make use of the entire config which corresponds to 'spec'
  },
  ["display-as", "unique-by"],
)
const matchOldListValueSpecString = object(
  {
    masked: boolean,
    copyable: boolean,
    pattern: string,
    "pattern-description": string,
    placeholder: string,
  },
  ["pattern", "pattern-description", "placeholder", "copyable", "masked"],
)

const matchOldListValueSpecEnum = object({
  values: array(string),
  "value-names": dictionary([string, string]),
})
const matchOldListValueSpecNumber = object(
  {
    range: string,
    integral: boolean,
    units: string,
    placeholder: anyOf(number, string),
  },
  ["units", "placeholder"],
)

// represents a spec for a list
const matchOldValueSpecList = every(
  object(
    {
      type: literals("list"),
      range: string, // '[0,1]' (inclusive) OR '[0,*)' (right unbounded), normal math rules
      default: anyOf(
        array(string),
        array(number),
        array(matchOldDefaultString),
        array(object),
      ),
      name: string,
      description: string,
      warning: string,
    },
    ["description", "warning"],
  ),
  anyOf(
    object({
      subtype: literals("string"),
      spec: matchOldListValueSpecString,
    }),
    object({
      subtype: literals("enum"),
      spec: matchOldListValueSpecEnum,
    }),
    object({
      subtype: literals("object"),
      spec: matchOldListValueSpecObject,
    }),
    object({
      subtype: literals("number"),
      spec: matchOldListValueSpecNumber,
    }),
  ),
)
type OldValueSpecList = typeof matchOldValueSpecList._TYPE

const matchOldValueSpecPointer = every(
  object({
    type: literal("pointer"),
  }),
  anyOf(
    object({
      subtype: literal("package"),
      target: literals("tor-key", "tor-address", "lan-address"),
      "package-id": string,
      interface: string,
    }),
    object({
      subtype: literal("package"),
      target: literals("config"),
      "package-id": string,
      selector: string,
      multi: boolean,
    }),
  ),
)
type OldValueSpecPointer = typeof matchOldValueSpecPointer._TYPE

export const matchOldValueSpec = anyOf(
  matchOldValueSpecString,
  matchOldValueSpecNumber,
  matchOldValueSpecBoolean,
  matchOldValueSpecObject,
  matchOldValueSpecEnum,
  matchOldValueSpecList,
  matchOldValueSpecUnion,
  matchOldValueSpecPointer,
)
type OldValueSpec = typeof matchOldValueSpec._TYPE

setMatchOldConfigSpec(dictionary([string, matchOldValueSpec]))

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
