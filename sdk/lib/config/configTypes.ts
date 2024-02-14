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
export type ValueSpecOf<T extends ValueType> = T extends "text"
  ? ValueSpecText
  : T extends "textarea"
    ? ValueSpecTextarea
    : T extends "number"
      ? ValueSpecNumber
      : T extends "color"
        ? ValueSpecColor
        : T extends "datetime"
          ? ValueSpecDatetime
          : T extends "toggle"
            ? ValueSpecToggle
            : T extends "select"
              ? ValueSpecSelect
              : T extends "multiselect"
                ? ValueSpecMultiselect
                : T extends "list"
                  ? ValueSpecList
                  : T extends "object"
                    ? ValueSpecObject
                    : T extends "file"
                      ? ValueSpecFile
                      : T extends "union"
                        ? ValueSpecUnion
                        : never

export interface ValueSpecText extends ListValueSpecText, WithStandalone {
  required: boolean
  default: DefaultString | null
  disabled: false | string
  generate: null | RandomString
  /** Immutable means it can only be configed at the first config then never again */
  immutable: boolean
}
export interface ValueSpecTextarea extends WithStandalone {
  type: "textarea"
  placeholder: string | null
  minLength: number | null
  maxLength: number | null
  required: boolean
  disabled: false | string
  /** Immutable means it can only be configed at the first config then never again */
  immutable: boolean
}

export type FilePath = {
  filePath: string
}
export interface ValueSpecNumber extends ListValueSpecNumber, WithStandalone {
  required: boolean
  default: number | null
  disabled: false | string
  /** Immutable means it can only be configed at the first config then never again */
  immutable: boolean
}
export interface ValueSpecColor extends WithStandalone {
  type: "color"
  required: boolean
  default: string | null
  disabled: false | string
  /** Immutable means it can only be configed at the first config then never again */
  immutable: boolean
}
export interface ValueSpecDatetime extends WithStandalone {
  type: "datetime"
  required: boolean
  inputmode: "date" | "time" | "datetime-local"
  min: string | null
  max: string | null
  default: string | null
  disabled: false | string
  /** Immutable means it can only be configed at the first config then never again */
  immutable: boolean
}
export interface ValueSpecSelect extends SelectBase, WithStandalone {
  type: "select"
  required: boolean
  default: string | null
  /**
   * Disabled:  false means that there is nothing disabled, good to modify
   *           string means that this is the message displayed and the whole thing is disabled
   *           string[] means that the options are disabled
   */
  disabled: false | string | string[]
  /** Immutable means it can only be configed at the first config then never again */
  immutable: boolean
}
export interface ValueSpecMultiselect extends SelectBase, WithStandalone {
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
  /** Immutable means it can only be configed at the first config then never again */
  immutable: boolean
}
export interface ValueSpecToggle extends WithStandalone {
  type: "toggle"
  default: boolean | null
  disabled: false | string
  /** Immutable means it can only be configed at the first config then never again */
  immutable: boolean
}
export interface ValueSpecUnion extends WithStandalone {
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
  /** Immutable means it can only be configed at the first config then never again */
  immutable: boolean
}
export interface ValueSpecFile extends WithStandalone {
  type: "file"
  extensions: string[]
  required: boolean
}
export interface ValueSpecObject extends WithStandalone {
  type: "object"
  spec: InputSpec
}
export interface WithStandalone {
  name: string
  description: string | null
  warning: string | null
}
export interface SelectBase {
  values: Record<string, string>
}
export type ListValueSpecType = "text" | "number" | "object"
/** represents a spec for the values of a list */
export type ListValueSpecOf<T extends ListValueSpecType> = T extends "text"
  ? ListValueSpecText
  : T extends "number"
  ? ListValueSpecNumber
  : T extends "object"
  ? ListValueSpecObject
  : never
/** represents a spec for a list */
export type ValueSpecList = ValueSpecListOf<ListValueSpecType>
export interface ValueSpecListOf<T extends ListValueSpecType>
  extends WithStandalone {
  type: "list"
  spec: ListValueSpecOf<T>
  minLength: number | null
  maxLength: number | null
  disabled: false | string
  default:
    | string[]
    | number[]
    | DefaultString[]
    | Record<string, unknown>[]
    | readonly string[]
    | readonly number[]
    | readonly DefaultString[]
    | readonly Record<string, unknown>[]
}
export interface Pattern {
  regex: string
  description: string
}
export interface ListValueSpecText {
  type: "text"
  patterns: Pattern[]
  minLength: number | null
  maxLength: number | null
  masked: boolean

  generate: null | RandomString
  inputmode: "text" | "email" | "tel" | "url"
  placeholder: string | null
}
export interface ListValueSpecNumber {
  type: "number"
  min: number | null
  max: number | null
  integer: boolean
  step: number | null
  units: string | null
  placeholder: string | null
}
export interface ListValueSpecObject {
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
