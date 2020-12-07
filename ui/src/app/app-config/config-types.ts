export interface ConfigSpec { [key: string]: ValueSpec }

export type ValueType = 'string' | 'number' | 'boolean' | 'enum' | 'list' | 'object' | 'pointer' | 'union'
export type ValueSpec = ValueSpecOf<ValueType>

// core spec types. These types provide the metadata for performing validations
export type ValueSpecOf<T extends ValueType> =
  T extends 'string'  ? ValueSpecString  :
  T extends 'number'  ? ValueSpecNumber  :
  T extends 'boolean' ? ValueSpecBoolean :
  T extends 'enum'    ? ValueSpecEnum    :
  T extends 'list'    ? ValueSpecList    :
  T extends 'object'  ? ValueSpecObject  :
  T extends 'pointer' ? ValueSpecPointer :
  T extends 'union'   ? ValueSpecUnion   :
  never

export interface ValueSpecString extends ListValueSpecString, WithStandalone {
  type: 'string'
  default?: DefaultString
  nullable: boolean
  masked: boolean
  copyable: boolean
}

export interface ValueSpecNumber extends ListValueSpecNumber, WithStandalone {
  type: 'number'
  nullable: boolean
  default?: number
}

export interface ValueSpecEnum extends ListValueSpecEnum, WithStandalone {
  type: 'enum'
  default: string
}

export interface ValueSpecBoolean extends WithStandalone {
  type: 'boolean'
  default: boolean
}

export interface ValueSpecUnion extends ListValueSpecUnion, WithStandalone {
  type: 'union'
}

export interface ValueSpecPointer extends WithStandalone {
  type: 'pointer'
  subtype: 'app' | 'system'
  target: 'lan-address' | 'tor-address' | 'config'
  'app-id': string
}

export interface ValueSpecObject extends ListValueSpecObject, WithStandalone {
  type: 'object'
  nullable: boolean
  nullByDefault: boolean
}

export interface WithStandalone {
  name: string
  description?: string
  changeWarning?: string
}

// no lists of booleans, lists, pointers
export type ListValueSpecType = 'string' | 'number' | 'enum' | 'object' | 'union'

// represents a spec for the values of a list
export type ListValueSpecOf<T extends ListValueSpecType> =
  T extends 'string' ? ListValueSpecString :
  T extends 'number' ? ListValueSpecNumber :
  T extends 'enum'   ? ListValueSpecEnum   :
  T extends 'object' ? ListValueSpecObject :
  T extends 'union'  ? ListValueSpecUnion  :
  never

// represents a spec for a list
export type ValueSpecList = ValueSpecListOf<ListValueSpecType>
export interface ValueSpecListOf<T extends ListValueSpecType> extends WithStandalone {
  type: 'list'
  subtype: T
  spec: ListValueSpecOf<T>
  range: string // '[0,1]' (inclusive) OR '[0,*)' (right unbounded), normal math rules
  default: string[] | number[] | DefaultString[] | object[]
}

// sometimes the type checker needs just a little bit of help
export function isValueSpecListOf<S extends ListValueSpecType> (t: ValueSpecList, s: S): t is ValueSpecListOf<S> {
  return t.subtype === s
}

export interface ListValueSpecString {
  pattern?: string
  patternDescription?: string
}

export interface ListValueSpecNumber {
  range: string
  integral: boolean
  units?: string
}

export interface ListValueSpecEnum {
  values: string[]
  valuesSet?: Set<string>
  valueNames: { [value: string]: string }
}

export interface ListValueSpecObject {
  spec: ConfigSpec //this is a mapped type of the config object at this level, replacing the object's values with specs on those values
  uniqueBy: UniqueBy //indicates whether duplicates can be permitted in the list
  displayAs?: string //this should be a handlebars template which can make use of the entire config which corresponds to 'spec'
}

export type UniqueBy = null | string | { any: UniqueBy[] } | { all: UniqueBy[] }

export interface ListValueSpecUnion {
  tag: UnionTagSpec
  variants: { [key: string]: ConfigSpec }
  displayAs?: string //this may be a handlebars template which can conditionally (on tag.id) make use of each union's entries, or if left blank will display as tag.id
  uniqueBy: UniqueBy
  default: string //this should be the variantName which one prefers a user to start with by default when creating a new union instance in a list
}

export interface UnionTagSpec {
  id: string //The name of the field containing one of the union variants
  name: string
  description?: string
  variantNames: { //the name of each variant
    [variant: string]: string
  }
}

export type DefaultString = string | { charset: string, len: number }
