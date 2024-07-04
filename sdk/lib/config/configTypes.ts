export type InputSpec = Record<string, ValueSpec>
export type ValueType =
  | "text"
  | "textarea"
  | "number"
  | "color"
  | "datetime"
  | "toggle"
  | "select"
  | "multiselect"
  | "list"
  | "object"
  | "file"
  | "union"
export type ValueSpec = ValueSpecOf<ValueType>
/** core spec types. These types provide the metadata for performing validations */
// prettier-ignore
export type ValueSpecOf<T extends ValueType> = 
  T extends "text" ? ValueSpecText : 
  T extends "textarea" ? ValueSpecTextarea : 
  T extends "number" ? ValueSpecNumber : 
  T extends "color" ? ValueSpecColor : 
  T extends "datetime" ? ValueSpecDatetime : 
  T extends "toggle" ? ValueSpecToggle : 
  T extends "select" ? ValueSpecSelect : 
  T extends "multiselect" ? ValueSpecMultiselect : 
  T extends "list" ? ValueSpecList : 
  T extends "object" ? ValueSpecObject : 
  T extends "file" ? ValueSpecFile : 
  T extends "union" ? ValueSpecUnion : 
  never

export type ValueSpecText = {
  name: string
  description: string | null
  warning: string | null

  type: "text"
  patterns: Pattern[]
  minLength: number | null
  maxLength: number | null
  masked: boolean

  inputmode: "text" | "email" | "tel" | "url"
  placeholder: string | null

  required: boolean
  default: DefaultString | null
  disabled: false | string
  generate: null | RandomString
  /** Immutable means it can only be configured at the first config then never again */
  immutable: boolean
}
export type ValueSpecTextarea = {
  name: string
  description: string | null
  warning: string | null

  type: "textarea"
  placeholder: string | null
  minLength: number | null
  maxLength: number | null
  required: boolean
  disabled: false | string
  /** Immutable means it can only be configured at the first config then never again */
  immutable: boolean
}

export type FilePath = {
  filePath: string
}
export type ValueSpecNumber = {
  type: "number"
  min: number | null
  max: number | null
  integer: boolean
  step: number | null
  units: string | null
  placeholder: string | null
  name: string
  description: string | null
  warning: string | null
  required: boolean
  default: number | null
  disabled: false | string
  /** Immutable means it can only be configured at the first config then never again */
  immutable: boolean
}
export type ValueSpecColor = {
  name: string
  description: string | null
  warning: string | null

  type: "color"
  required: boolean
  default: string | null
  disabled: false | string
  /** Immutable means it can only be configured at the first config then never again */
  immutable: boolean
}
export type ValueSpecDatetime = {
  name: string
  description: string | null
  warning: string | null
  type: "datetime"
  required: boolean
  inputmode: "date" | "time" | "datetime-local"
  min: string | null
  max: string | null
  default: string | null
  disabled: false | string
  /** Immutable means it can only be configured at the first config then never again */
  immutable: boolean
}
export type ValueSpecSelect = {
  values: Record<string, string>
  name: string
  description: string | null
  warning: string | null
  type: "select"
  required: boolean
  default: string | null
  /**
   * Disabled:  false means that there is nothing disabled, good to modify
   *           string means that this is the message displayed and the whole thing is disabled
   *           string[] means that the options are disabled
   */
  disabled: false | string | string[]
  /** Immutable means it can only be configured at the first config then never again */
  immutable: boolean
}
export type ValueSpecMultiselect = {
  values: Record<string, string>

  name: string
  description: string | null
  warning: string | null

  type: "multiselect"
  minLength: number | null
  maxLength: number | null
  /**
   * Disabled:  false means that there is nothing disabled, good to modify
   *           string means that this is the message displayed and the whole thing is disabled
   *           string[] means that the options are disabled
   */
  disabled: false | string | string[]
  default: string[]
  /** Immutable means it can only be configured at the first config then never again */
  immutable: boolean
}
export type ValueSpecToggle = {
  name: string
  description: string | null
  warning: string | null

  type: "toggle"
  default: boolean | null
  disabled: false | string
  /** Immutable means it can only be configured at the first config then never again */
  immutable: boolean
}
export type ValueSpecUnion = {
  name: string
  description: string | null
  warning: string | null

  type: "union"
  variants: Record<
    string,
    {
      name: string
      spec: InputSpec
    }
  >
  /**
   * Disabled:  false means that there is nothing disabled, good to modify
   *           string means that this is the message displayed and the whole thing is disabled
   *           string[] means that the options are disabled
   */
  disabled: false | string | string[]
  required: boolean
  default: string | null
  /** Immutable means it can only be configured at the first config then never again */
  immutable: boolean
}
export type ValueSpecFile = {
  name: string
  description: string | null
  warning: string | null
  type: "file"
  extensions: string[]
  required: boolean
}
export type ValueSpecObject = {
  name: string
  description: string | null
  warning: string | null
  type: "object"
  spec: InputSpec
}
export type ListValueSpecType = "text" | "object"
/** represents a spec for the values of a list */
// prettier-ignore
export type ListValueSpecOf<T extends ListValueSpecType> = 
  T extends "text" ? ListValueSpecText :
  T extends "object" ? ListValueSpecObject :
  never
/** represents a spec for a list */
export type ValueSpecList = ValueSpecListOf<ListValueSpecType>
export type ValueSpecListOf<T extends ListValueSpecType> = {
  name: string
  description: string | null
  warning: string | null
  type: "list"
  spec: ListValueSpecOf<T>
  minLength: number | null
  maxLength: number | null
  disabled: false | string
  default:
    | string[]
    | DefaultString[]
    | Record<string, unknown>[]
    | readonly string[]
    | readonly DefaultString[]
    | readonly Record<string, unknown>[]
}
export type Pattern = {
  regex: string
  description: string
}
export type ListValueSpecText = {
  type: "text"
  patterns: Pattern[]
  minLength: number | null
  maxLength: number | null
  masked: boolean

  generate: null | RandomString
  inputmode: "text" | "email" | "tel" | "url"
  placeholder: string | null
}

export type ListValueSpecObject = {
  type: "object"
  /** this is a mapped type of the config object at this level, replacing the object's values with specs on those values */
  spec: InputSpec
  /** indicates whether duplicates can be permitted in the list */
  uniqueBy: UniqueBy
  /** this should be a handlebars template which can make use of the entire config which corresponds to 'spec' */
  displayAs: string | null
}
export type UniqueBy =
  | null
  | string
  | {
      any: readonly UniqueBy[] | UniqueBy[]
    }
  | {
      all: readonly UniqueBy[] | UniqueBy[]
    }
export type DefaultString = string | RandomString
export type RandomString = {
  charset: string
  len: number
}
// sometimes the type checker needs just a little bit of help
export function isValueSpecListOf<S extends ListValueSpecType>(
  t: ValueSpec,
  s: S,
): t is ValueSpecListOf<S> & { spec: ListValueSpecOf<S> } {
  return "spec" in t && t.spec.type === s
}
export const unionSelectKey = "unionSelectKey" as const
export type UnionSelectKey = typeof unionSelectKey

export const unionValueKey = "unionValueKey" as const
export type UnionValueKey = typeof unionValueKey
