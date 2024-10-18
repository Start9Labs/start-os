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
  | "hidden"
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
  T extends "hidden" ? ValueSpecHidden :
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
  disabled: false | string | string[]
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
  disabled: false | string | string[]
  default: string[]
  immutable: boolean
}
export type ValueSpecToggle = {
  name: string
  description: string | null
  warning: string | null

  type: "toggle"
  default: boolean | null
  disabled: false | string
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
  disabled: false | string | string[]
  required: boolean
  default: string | null
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
export type ValueSpecHidden = {
  type: "hidden"
}
export type ListValueSpecType = "text" | "object"
// prettier-ignore
export type ListValueSpecOf<T extends ListValueSpecType> = 
  T extends "text" ? ListValueSpecText :
  T extends "object" ? ListValueSpecObject :
  never
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
  spec: InputSpec
  uniqueBy: UniqueBy
  displayAs: string | null
}
// TODO Aiden do we really want this expressivity? Why not the below. Also what's with the "readonly" portion?
// export type UniqueBy = null | string | { any: string[] } | { all: string[] }

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
