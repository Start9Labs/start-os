import { InputSpec, LazyBuild } from "./inputSpec"
import { List } from "./list"
import { Variants } from "./variants"
import {
  FilePath,
  Pattern,
  RandomString,
  ValueSpec,
  ValueSpecDatetime,
  ValueSpecHidden,
  ValueSpecText,
  ValueSpecTextarea,
} from "../inputSpecTypes"
import { DefaultString } from "../inputSpecTypes"
import { _, once } from "../../../util"
import {
  Parser,
  any,
  anyOf,
  arrayOf,
  boolean,
  literal,
  literals,
  number,
  object,
  string,
  unknown,
} from "ts-matches"

export type RequiredDefault<A> =
  | false
  | {
      default: A | null
    }

function requiredLikeToAbove<Input extends RequiredDefault<A>, A>(
  requiredLike: Input,
) {
  // prettier-ignore
  return {
    required: (typeof requiredLike === 'object' ? true : requiredLike) as (
      Input extends { default: unknown} ? true:
      Input extends true ? true :
      false
    ),
    default:(typeof requiredLike === 'object' ? requiredLike.default : null) as (
      Input extends { default: infer Default } ? Default :
      null
    )
  };
}
type AsRequired<Type, MaybeRequiredType> = MaybeRequiredType extends
  | { default: unknown }
  | never
  ? Type
  : Type | null | undefined

const testForAsRequiredParser = once(
  () => object({ required: object({ default: unknown }) }).test,
)
function asRequiredParser<
  Type,
  Input,
  Return extends
    | Parser<unknown, Type>
    | Parser<unknown, Type | null | undefined>,
>(parser: Parser<unknown, Type>, input: Input): Return {
  if (testForAsRequiredParser()(input)) return parser as any
  return parser.optional() as any
}

export class Value<Type, Store> {
  protected constructor(
    public build: LazyBuild<Store, ValueSpec>,
    public validator: Parser<unknown, Type>,
  ) {}
  static toggle(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    default: boolean
    /**
     * @description Once set, the value can never be changed.
     * @default false
     */
    immutable?: boolean
  }) {
    return new Value<boolean, never>(
      async () => ({
        description: null,
        warning: null,
        type: "toggle" as const,
        disabled: false,
        immutable: a.immutable ?? false,
        ...a,
      }),
      boolean,
    )
  }
  static dynamicToggle<Store = never>(
    a: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        default: boolean
        disabled?: false | string
      }
    >,
  ) {
    return new Value<boolean, Store>(
      async (options) => ({
        description: null,
        warning: null,
        type: "toggle" as const,
        disabled: false,
        immutable: false,
        ...(await a(options)),
      }),
      boolean,
    )
  }
  static text<Required extends RequiredDefault<DefaultString>>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description Determines if the field is required. If so, optionally provide a default value.
     * @type { false | { default: string | RandomString | null } }
     * @example required: false
     * @example required: { default: null }
     * @example required: { default: 'World' }
     * @example required: { default: { charset: 'abcdefg', len: 16 } }
     */
    required: Required
    /**
     * @description Mask (aka camouflage) text input with dots: ● ● ●
     * @default false
     */
    masked?: boolean
    placeholder?: string | null
    minLength?: number | null
    maxLength?: number | null
    /**
     * @description A list of regular expressions to which the text must conform to pass validation. A human readable description is provided in case the validation fails.
     * @default []
     * @example
     * ```
      [
        {
          regex: "[a-z]",
          description: "May only contain lower case letters from the English alphabet."
        }
      ]
     * ```
     */
    patterns?: Pattern[]
    /**
     * @description Informs the browser how to behave and which keyboard to display on mobile
     * @default "text"
     */
    inputmode?: ValueSpecText["inputmode"]
    /**
     * @description Once set, the value can never be changed.
     * @default false
     */
    immutable?: boolean
    /**
     * @description Displays a button that will generate a random string according to the provided charset and len attributes.
     */
    generate?: RandomString | null
  }) {
    return new Value<AsRequired<string, Required>, never>(
      async () => ({
        type: "text" as const,
        description: null,
        warning: null,
        masked: false,
        placeholder: null,
        minLength: null,
        maxLength: null,
        patterns: [],
        inputmode: "text",
        disabled: false,
        immutable: a.immutable ?? false,
        generate: a.generate ?? null,
        ...a,
        ...requiredLikeToAbove(a.required),
      }),
      asRequiredParser(string, a),
    )
  }
  static dynamicText<Store = never>(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        required: RequiredDefault<DefaultString>
        masked?: boolean
        placeholder?: string | null
        minLength?: number | null
        maxLength?: number | null
        patterns?: Pattern[]
        inputmode?: ValueSpecText["inputmode"]
        disabled?: string | false
        generate?: null | RandomString
      }
    >,
  ) {
    return new Value<string | null | undefined, Store>(async (options) => {
      const a = await getA(options)
      return {
        type: "text" as const,
        description: null,
        warning: null,
        masked: false,
        placeholder: null,
        minLength: null,
        maxLength: null,
        patterns: [],
        inputmode: "text",
        disabled: false,
        immutable: false,
        generate: a.generate ?? null,
        ...a,
        ...requiredLikeToAbove(a.required),
      }
    }, string.optional())
  }
  static textarea(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description Unlike other "required" fields, for textarea this is a simple boolean.
     */
    required: boolean
    minLength?: number | null
    maxLength?: number | null
    placeholder?: string | null
    /**
     * @description Once set, the value can never be changed.
     * @default false
     */
    immutable?: boolean
  }) {
    return new Value<string, never>(async () => {
      const built: ValueSpecTextarea = {
        description: null,
        warning: null,
        minLength: null,
        maxLength: null,
        placeholder: null,
        type: "textarea" as const,
        disabled: false,
        immutable: a.immutable ?? false,
        ...a,
      }
      return built
    }, string)
  }
  static dynamicTextarea<Store = never>(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        required: boolean
        minLength?: number | null
        maxLength?: number | null
        placeholder?: string | null
        disabled?: false | string
      }
    >,
  ) {
    return new Value<string, Store>(async (options) => {
      const a = await getA(options)
      return {
        description: null,
        warning: null,
        minLength: null,
        maxLength: null,
        placeholder: null,
        type: "textarea" as const,
        disabled: false,
        immutable: false,
        ...a,
      }
    }, string)
  }
  static number<Required extends RequiredDefault<number>>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description Determines if the field is required. If so, optionally provide a default value.
     * @type { false | { default: number | null } }
     * @example required: false
     * @example required: { default: null }
     * @example required: { default: 7 }
     */
    required: Required
    min?: number | null
    max?: number | null
    /**
     * @description How much does the number increase/decrease when using the arrows provided by the browser.
     * @default 1
     */
    step?: number | null
    /**
     * @description Requires the number to be an integer.
     */
    integer: boolean
    /**
     * @description Optionally display units to the right of the input box.
     */
    units?: string | null
    placeholder?: string | null
    /**
     * @description Once set, the value can never be changed.
     * @default false
     */
    immutable?: boolean
  }) {
    return new Value<AsRequired<number, Required>, never>(
      () => ({
        type: "number" as const,
        description: null,
        warning: null,
        min: null,
        max: null,
        step: null,
        units: null,
        placeholder: null,
        disabled: false,
        immutable: a.immutable ?? false,
        ...a,
        ...requiredLikeToAbove(a.required),
      }),
      asRequiredParser(number, a),
    )
  }
  static dynamicNumber<Store = never>(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        required: RequiredDefault<number>
        min?: number | null
        max?: number | null
        step?: number | null
        integer: boolean
        units?: string | null
        placeholder?: string | null
        disabled?: false | string
      }
    >,
  ) {
    return new Value<number | null | undefined, Store>(async (options) => {
      const a = await getA(options)
      return {
        type: "number" as const,
        description: null,
        warning: null,
        min: null,
        max: null,
        step: null,
        units: null,
        placeholder: null,
        disabled: false,
        immutable: false,
        ...a,
        ...requiredLikeToAbove(a.required),
      }
    }, number.optional())
  }
  static color<Required extends RequiredDefault<string>>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description Determines if the field is required. If so, optionally provide a default value.
     * @type { false | { default: string | null } }
     * @example required: false
     * @example required: { default: null }
     * @example required: { default: 'ffffff' }
     */
    required: Required
    /**
     * @description Once set, the value can never be changed.
     * @default false
     */
    immutable?: boolean
  }) {
    return new Value<AsRequired<string, Required>, never>(
      () => ({
        type: "color" as const,
        description: null,
        warning: null,
        disabled: false,
        immutable: a.immutable ?? false,
        ...a,
        ...requiredLikeToAbove(a.required),
      }),

      asRequiredParser(string, a),
    )
  }

  static dynamicColor<Store = never>(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        required: RequiredDefault<string>
        disabled?: false | string
      }
    >,
  ) {
    return new Value<string | null | undefined, Store>(async (options) => {
      const a = await getA(options)
      return {
        type: "color" as const,
        description: null,
        warning: null,
        disabled: false,
        immutable: false,
        ...a,
        ...requiredLikeToAbove(a.required),
      }
    }, string.optional())
  }
  static datetime<Required extends RequiredDefault<string>>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description Determines if the field is required. If so, optionally provide a default value.
     * @type { false | { default: string | null } }
     * @example required: false
     * @example required: { default: null }
     * @example required: { default: '1985-12-16 18:00:00.000' }
     */
    required: Required
    /**
     * @description Informs the browser how to behave and which date/time component to display.
     * @default "datetime-local"
     */
    inputmode?: ValueSpecDatetime["inputmode"]
    min?: string | null
    max?: string | null
    /**
     * @description Once set, the value can never be changed.
     * @default false
     */
    immutable?: boolean
  }) {
    return new Value<AsRequired<string, Required>, never>(
      () => ({
        type: "datetime" as const,
        description: null,
        warning: null,
        inputmode: "datetime-local",
        min: null,
        max: null,
        step: null,
        disabled: false,
        immutable: a.immutable ?? false,
        ...a,
        ...requiredLikeToAbove(a.required),
      }),
      asRequiredParser(string, a),
    )
  }
  static dynamicDatetime<Store = never>(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        required: RequiredDefault<string>
        inputmode?: ValueSpecDatetime["inputmode"]
        min?: string | null
        max?: string | null
        disabled?: false | string
      }
    >,
  ) {
    return new Value<string | null | undefined, Store>(async (options) => {
      const a = await getA(options)
      return {
        type: "datetime" as const,
        description: null,
        warning: null,
        inputmode: "datetime-local",
        min: null,
        max: null,
        disabled: false,
        immutable: false,
        ...a,
        ...requiredLikeToAbove(a.required),
      }
    }, string.optional())
  }
  static select<
    Required extends RequiredDefault<string>,
    Values extends Record<string, string>,
  >(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description Determines if the field is required. If so, optionally provide a default value from the list of values.
     * @type { false | { default: string | null } }
     * @example required: false
     * @example required: { default: null }
     * @example required: { default: 'radio1' }
     */
    required: Required
    /**
     * @description A mapping of unique radio options to their human readable display format.
     * @example
     * ```
      {
        radio1: "Radio 1"
        radio2: "Radio 2"
        radio3: "Radio 3"
      }
     * ```
     */
    values: Values
    /**
     * @description Once set, the value can never be changed.
     * @default false
     */
    immutable?: boolean
  }) {
    return new Value<AsRequired<keyof Values, Required>, never>(
      () => ({
        description: null,
        warning: null,
        type: "select" as const,
        disabled: false,
        immutable: a.immutable ?? false,
        ...a,
        ...requiredLikeToAbove(a.required),
      }),
      asRequiredParser(
        anyOf(
          ...Object.keys(a.values).map((x: keyof Values & string) =>
            literal(x),
          ),
        ),
        a,
      ) as any,
    )
  }
  static dynamicSelect<Store = never>(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        required: RequiredDefault<string>
        values: Record<string, string>
        disabled?: false | string | string[]
      }
    >,
  ) {
    return new Value<string | null | undefined, Store>(async (options) => {
      const a = await getA(options)
      return {
        description: null,
        warning: null,
        type: "select" as const,
        disabled: false,
        immutable: false,
        ...a,
        ...requiredLikeToAbove(a.required),
      }
    }, string.optional())
  }
  static multiselect<Values extends Record<string, string>>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description A simple list of which options should be checked by default.
     */
    default: string[]
    /**
     * @description A mapping of checkbox options to their human readable display format.
     * @example
     * ```
      {
        option1: "Option 1"
        option2: "Option 2"
        option3: "Option 3"
      }
     * ```
     */
    values: Values
    minLength?: number | null
    maxLength?: number | null
    /**
     * @description Once set, the value can never be changed.
     * @default false
     */
    immutable?: boolean
  }) {
    return new Value<(keyof Values)[], never>(
      () => ({
        type: "multiselect" as const,
        minLength: null,
        maxLength: null,
        warning: null,
        description: null,
        disabled: false,
        immutable: a.immutable ?? false,
        ...a,
      }),
      arrayOf(
        literals(...(Object.keys(a.values) as any as [keyof Values & string])),
      ),
    )
  }
  static dynamicMultiselect<Store = never>(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        default: string[]
        values: Record<string, string>
        minLength?: number | null
        maxLength?: number | null
        disabled?: false | string | string[]
      }
    >,
  ) {
    return new Value<string[], Store>(async (options) => {
      const a = await getA(options)
      return {
        type: "multiselect" as const,
        minLength: null,
        maxLength: null,
        warning: null,
        description: null,
        disabled: false,
        immutable: false,
        ...a,
      }
    }, arrayOf(string))
  }
  static object<Type extends Record<string, any>, Store>(
    a: {
      name: string
      description?: string | null
    },
    spec: InputSpec<Type, Store>,
  ) {
    return new Value<Type, Store>(async (options) => {
      const built = await spec.build(options as any)
      return {
        type: "object" as const,
        description: null,
        warning: null,
        ...a,
        spec: built,
      }
    }, spec.validator)
  }
  // static file<Store>(a: {
  //   name: string
  //   description?: string | null
  //   extensions: string[]
  //   required: boolean
  // }) {
  //   const buildValue = {
  //     type: "file" as const,
  //     description: null,
  //     warning: null,
  //     ...a,
  //   }
  //   return new Value<FilePath, Store>(
  //     () => ({
  //       ...buildValue,
  //     }),
  //     asRequiredParser(object({ filePath: string }), a),
  //   )
  // }
  // static dynamicFile<Required extends boolean, Store>(
  //   a: LazyBuild<
  //     Store,
  //     {
  //       name: string
  //       description?: string | null
  //       warning?: string | null
  //       extensions: string[]
  //       required: Required
  //     }
  //   >,
  // ) {
  //   return new Value<string | null | undefined, Store>(
  //     async (options) => ({
  //       type: "file" as const,
  //       description: null,
  //       warning: null,
  //       ...(await a(options)),
  //     }),
  //     string.optional(),
  //   )
  // }
  static union<Required extends RequiredDefault<string>, Type, Store>(
    a: {
      name: string
      description?: string | null
      /** Presents a warning prompt before permitting the value to change. */
      warning?: string | null
      /**
       * @description Determines if the field is required. If so, optionally provide a default value from the list of variants.
       * @type { false | { default: string | null } }
       * @example required: false
       * @example required: { default: null }
       * @example required: { default: 'variant1' }
       */
      required: Required
      /**
       * @description Once set, the value can never be changed.
       * @default false
       */
      immutable?: boolean
    },
    aVariants: Variants<Type, Store>,
  ) {
    return new Value<AsRequired<Type, Required>, Store>(
      async (options) => ({
        type: "union" as const,
        description: null,
        warning: null,
        disabled: false,
        ...a,
        variants: await aVariants.build(options as any),
        ...requiredLikeToAbove(a.required),
        immutable: a.immutable ?? false,
      }),
      asRequiredParser(aVariants.validator, a),
    )
  }
  static filteredUnion<
    Required extends RequiredDefault<string>,
    Type extends Record<string, any>,
    Store = never,
  >(
    getDisabledFn: LazyBuild<Store, string[] | false | string>,
    a: {
      name: string
      description?: string | null
      warning?: string | null
      required: Required
    },
    aVariants: Variants<Type, Store> | Variants<Type, never>,
  ) {
    return new Value<AsRequired<Type, Required>, Store>(
      async (options) => ({
        type: "union" as const,
        description: null,
        warning: null,
        ...a,
        variants: await aVariants.build(options as any),
        ...requiredLikeToAbove(a.required),
        disabled: (await getDisabledFn(options)) || false,
        immutable: false,
      }),
      asRequiredParser(aVariants.validator, a),
    )
  }
  static dynamicUnion<
    Required extends RequiredDefault<string>,
    Type extends Record<string, any>,
    Store = never,
  >(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        required: Required
        disabled: string[] | false | string
      }
    >,
    aVariants: Variants<Type, Store> | Variants<Type, never>,
  ) {
    return new Value<Type | null | undefined, Store>(async (options) => {
      const newValues = await getA(options)
      return {
        type: "union" as const,
        description: null,
        warning: null,
        ...newValues,
        variants: await aVariants.build(options as any),
        ...requiredLikeToAbove(newValues.required),
        immutable: false,
      }
    }, aVariants.validator.optional())
  }

  static list<Type, Store>(a: List<Type, Store>) {
    return new Value<Type, Store>((options) => a.build(options), a.validator)
  }

  static hidden<T>(parser: Parser<unknown, T> = any) {
    return new Value<T, never>(async () => {
      const built: ValueSpecHidden = {
        type: "hidden" as const,
      }
      return built
    }, parser)
  }

  /**
   * Use this during the times that the input needs a more specific type.
   * Used in types that the value/ variant/ list/ inputSpec is constructed somewhere else.
  ```ts
  const a = InputSpec.text({
    name: "a",
    required: false,
  })

  return InputSpec.of<Store>()({
    myValue: a.withStore(),
  })
  ```
   */
  withStore<NewStore extends Store extends never ? any : Store>() {
    return this as any as Value<Type, NewStore>
  }
}
