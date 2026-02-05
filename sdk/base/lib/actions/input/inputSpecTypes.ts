/**
 * @module inputSpecTypes
 *
 * This module defines the type specifications for action input form fields.
 * These types describe the shape of form fields that appear in the StartOS UI
 * when users interact with service actions.
 *
 * Developers typically don't create these types directly - instead, use the
 * `Value` class methods (e.g., `Value.text()`, `Value.select()`) which generate
 * these specifications with proper defaults and validation.
 *
 * @see {@link Value} for the builder API
 */

/**
 * A complete input specification - a record mapping field names to their specifications.
 * This is the top-level type for an action's input form.
 */
export type InputSpec = Record<string, ValueSpec>

/**
 * All available input field types.
 *
 * - `text` - Single-line text input
 * - `textarea` - Multi-line text input
 * - `number` - Numeric input with optional min/max/step
 * - `color` - Color picker
 * - `datetime` - Date and/or time picker
 * - `toggle` - Boolean on/off switch
 * - `select` - Single-selection dropdown/radio
 * - `multiselect` - Multiple-selection checkboxes
 * - `list` - Dynamic list of items (text or objects)
 * - `object` - Nested group of fields (sub-form)
 * - `file` - File upload
 * - `union` - Conditional fields based on selection (discriminated union)
 * - `hidden` - Hidden field (not displayed to user)
 */
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

/** Union type of all possible value specifications */
export type ValueSpec = ValueSpecOf<ValueType>

/**
 * Maps a ValueType to its corresponding specification type.
 * Core spec types that provide metadata for validation and UI rendering.
 */
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

/**
 * Specification for a single-line text input field.
 * Use `Value.text()` to create this specification.
 */
export type ValueSpecText = {
  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes (requires user confirmation) */
  warning: string | null

  type: "text"
  /** Regex patterns the value must match, with descriptions for validation errors */
  patterns: Pattern[]
  /** Minimum character length */
  minLength: number | null
  /** Maximum character length */
  maxLength: number | null
  /** If true, displays input as dots (●●●) for sensitive data like passwords */
  masked: boolean

  /** Browser input mode hint for mobile keyboards */
  inputmode: "text" | "email" | "tel" | "url"
  /** Placeholder text shown when the field is empty */
  placeholder: string | null

  /** If true, the field cannot be left empty */
  required: boolean
  /** Default value (can be a string or random string generator) */
  default: DefaultString | null
  /** If string, the field is disabled with this message explaining why */
  disabled: false | string
  /** Configuration for "Generate" button that creates random strings */
  generate: null | RandomString
  /** If true, the value cannot be changed after initial set */
  immutable: boolean
}

/**
 * Specification for a multi-line text area input field.
 * Use `Value.textarea()` to create this specification.
 */
export type ValueSpecTextarea = {
  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes */
  warning: string | null

  type: "textarea"
  /** Regex patterns the value must match */
  patterns: Pattern[]
  /** Placeholder text shown when the field is empty */
  placeholder: string | null
  /** Minimum character length */
  minLength: number | null
  /** Maximum character length */
  maxLength: number | null
  /** Minimum visible rows before scrolling */
  minRows: number
  /** Maximum visible rows before scrolling */
  maxRows: number
  /** If true, the field cannot be left empty */
  required: boolean
  /** Default value */
  default: string | null
  /** If string, the field is disabled with this message */
  disabled: false | string
  /** If true, the value cannot be changed after initial set */
  immutable: boolean
}

/**
 * Specification for a numeric input field.
 * Use `Value.number()` to create this specification.
 */
export type ValueSpecNumber = {
  type: "number"
  /** Minimum allowed value */
  min: number | null
  /** Maximum allowed value */
  max: number | null
  /** If true, only whole numbers are allowed */
  integer: boolean
  /** Increment/decrement step for arrow controls */
  step: number | null
  /** Unit label displayed after the input (e.g., "MB", "seconds") */
  units: string | null
  /** Placeholder text shown when the field is empty */
  placeholder: string | null
  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes */
  warning: string | null
  /** If true, the field cannot be left empty */
  required: boolean
  /** Default value */
  default: number | null
  /** If string, the field is disabled with this message */
  disabled: false | string
  /** If true, the value cannot be changed after initial set */
  immutable: boolean
}

/**
 * Specification for a color picker field.
 * Use `Value.color()` to create this specification.
 */
export type ValueSpecColor = {
  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes */
  warning: string | null

  type: "color"
  /** If true, a color must be selected */
  required: boolean
  /** Default color value (hex format, e.g., "ffffff") */
  default: string | null
  /** If string, the field is disabled with this message */
  disabled: false | string
  /** If true, the value cannot be changed after initial set */
  immutable: boolean
}

/**
 * Specification for a date/time picker field.
 * Use `Value.datetime()` to create this specification.
 */
export type ValueSpecDatetime = {
  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes */
  warning: string | null
  type: "datetime"
  /** If true, the field cannot be left empty */
  required: boolean
  /** Type of datetime picker to display */
  inputmode: "date" | "time" | "datetime-local"
  /** Minimum allowed date/time */
  min: string | null
  /** Maximum allowed date/time */
  max: string | null
  /** Default value */
  default: string | null
  /** If string, the field is disabled with this message */
  disabled: false | string
  /** If true, the value cannot be changed after initial set */
  immutable: boolean
}

/**
 * Specification for a single-selection dropdown or radio button group.
 * Use `Value.select()` to create this specification.
 */
export type ValueSpecSelect = {
  /** Map of option values to their display labels */
  values: Record<string, string>
  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes */
  warning: string | null
  type: "select"
  /** Default selected option key */
  default: string | null
  /** Disabled state: false=enabled, string=disabled with message, string[]=specific options disabled */
  disabled: false | string | string[]
  /** If true, the value cannot be changed after initial set */
  immutable: boolean
}

/**
 * Specification for a multiple-selection checkbox group.
 * Use `Value.multiselect()` to create this specification.
 */
export type ValueSpecMultiselect = {
  /** Map of option values to their display labels */
  values: Record<string, string>

  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes */
  warning: string | null

  type: "multiselect"
  /** Minimum number of selections required */
  minLength: number | null
  /** Maximum number of selections allowed */
  maxLength: number | null
  /** Disabled state: false=enabled, string=disabled with message, string[]=specific options disabled */
  disabled: false | string | string[]
  /** Default selected option keys */
  default: string[]
  /** If true, the value cannot be changed after initial set */
  immutable: boolean
}

/**
 * Specification for a boolean toggle switch.
 * Use `Value.toggle()` to create this specification.
 */
export type ValueSpecToggle = {
  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes */
  warning: string | null

  type: "toggle"
  /** Default value (on/off) */
  default: boolean | null
  /** If string, the field is disabled with this message */
  disabled: false | string
  /** If true, the value cannot be changed after initial set */
  immutable: boolean
}
/**
 * Specification for a discriminated union field (conditional sub-forms).
 * Shows different fields based on which variant is selected.
 * Use `Value.union()` with `Variants.of()` to create this specification.
 */
export type ValueSpecUnion = {
  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes */
  warning: string | null

  type: "union"
  /** Map of variant keys to their display names and nested field specifications */
  variants: Record<
    string,
    {
      /** Display name for this variant option */
      name: string
      /** Fields to show when this variant is selected */
      spec: InputSpec
    }
  >
  /** Disabled state: false=enabled, string=disabled with message, string[]=specific variants disabled */
  disabled: false | string | string[]
  /** Default selected variant key */
  default: string | null
  /** If true, the value cannot be changed after initial set */
  immutable: boolean
}

/**
 * Specification for a file upload field.
 * Use `Value.file()` to create this specification.
 */
export type ValueSpecFile = {
  /** Display label for the field */
  name: string
  /** Help text shown below the field */
  description: string | null
  /** Warning message shown when the value changes */
  warning: string | null
  type: "file"
  /** Allowed file extensions (e.g., [".json", ".yaml"]) */
  extensions: string[]
  /** If true, a file must be uploaded */
  required: boolean
}

/**
 * Specification for a nested object (sub-form / field group).
 * Use `Value.object()` to create this specification.
 */
export type ValueSpecObject = {
  /** Display label for the field group */
  name: string
  /** Help text shown below the field group */
  description: string | null
  /** Warning message (not typically used for objects) */
  warning: string | null
  type: "object"
  /** Nested field specifications */
  spec: InputSpec
}

/**
 * Specification for a hidden field (not displayed in the UI).
 * Use `Value.hidden()` to create this specification.
 * Useful for storing internal state that shouldn't be user-editable.
 */
export type ValueSpecHidden = {
  type: "hidden"
}

/** Types of items that can appear in a list */
export type ListValueSpecType = "text" | "object"

/** Maps a list item type to its specification */
// prettier-ignore
export type ListValueSpecOf<T extends ListValueSpecType> =
  T extends "text" ? ListValueSpecText :
  T extends "object" ? ListValueSpecObject :
  never

/** Union of all list specification types */
export type ValueSpecList = ValueSpecListOf<ListValueSpecType>

/**
 * Specification for a dynamic list of items.
 * Use `Value.list()` with `List.text()` or `List.obj()` to create this specification.
 */
export type ValueSpecListOf<T extends ListValueSpecType> = {
  /** Display label for the list field */
  name: string
  /** Help text shown below the list */
  description: string | null
  /** Warning message shown when items change */
  warning: string | null
  type: "list"
  /** Specification for individual list items */
  spec: ListValueSpecOf<T>
  /** Minimum number of items required */
  minLength: number | null
  /** Maximum number of items allowed */
  maxLength: number | null
  /** If string, the list is disabled with this message */
  disabled: false | string
  /** Default list items */
  default:
    | string[]
    | DefaultString[]
    | Record<string, unknown>[]
    | readonly string[]
    | readonly DefaultString[]
    | readonly Record<string, unknown>[]
}

/**
 * A validation pattern with a regex and human-readable description.
 * Used to validate text input and provide meaningful error messages.
 */
export type Pattern = {
  /** Regular expression pattern (as a string) */
  regex: string
  /** Human-readable description shown when validation fails */
  description: string
}

/**
 * Specification for text items within a list.
 * Created via `List.text()`.
 */
export type ListValueSpecText = {
  type: "text"
  /** Regex patterns each item must match */
  patterns: Pattern[]
  /** Minimum character length per item */
  minLength: number | null
  /** Maximum character length per item */
  maxLength: number | null
  /** If true, displays items as dots (●●●) */
  masked: boolean

  /** Configuration for "Generate" button */
  generate: null | RandomString
  /** Browser input mode hint */
  inputmode: "text" | "email" | "tel" | "url"
  /** Placeholder text for each item */
  placeholder: string | null
}

/**
 * Specification for object items within a list.
 * Created via `List.obj()`.
 */
export type ListValueSpecObject = {
  type: "object"
  /** Field specification for each object in the list */
  spec: InputSpec
  /** Constraint for ensuring unique items in the list */
  uniqueBy: UniqueBy
  /** Template string for how to display each item in the list (e.g., "{name} - {email}") */
  displayAs: string | null
}

/**
 * Defines how to determine uniqueness for list items.
 * - `null` - No uniqueness constraint
 * - `string` - Field name that must be unique (e.g., "email")
 * - `{ any: UniqueBy[] }` - Any of the specified constraints must be unique
 * - `{ all: UniqueBy[] }` - All of the specified constraints combined must be unique
 */
export type UniqueBy =
  | null
  | string
  | {
      any: readonly UniqueBy[] | UniqueBy[]
    }
  | {
      all: readonly UniqueBy[] | UniqueBy[]
    }

/**
 * Default value for a text field - either a literal string or a random string generator.
 */
export type DefaultString = string | RandomString

/**
 * Configuration for generating random strings (e.g., passwords, tokens).
 * Used with `Value.text({ generate: ... })` to show a "Generate" button.
 */
export type RandomString = {
  /** Characters to use when generating (e.g., "abcdefghijklmnopqrstuvwxyz0123456789") */
  charset: string
  /** Length of the generated string */
  len: number
}

/**
 * Type guard to check if a ValueSpec is a list of a specific item type.
 *
 * @param t - The value specification to check
 * @param s - The expected list item type ("text" or "object")
 * @returns True if the spec is a list of the specified type
 */
export function isValueSpecListOf<S extends ListValueSpecType>(
  t: ValueSpec,
  s: S,
): t is ValueSpecListOf<S> & { spec: ListValueSpecOf<S> } {
  return "spec" in t && t.spec.type === s
}
