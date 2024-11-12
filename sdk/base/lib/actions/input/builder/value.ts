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

type AsRequired<T, Required extends boolean> = Required extends true
  ? T
  : T | null | undefined

const testForAsRequiredParser = once(
  () => object({ required: literal(true) }).test,
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
  static text<Required extends boolean>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * provide a default value.
     * @type { string | RandomString | null }
     * @example default: null
     * @example default: 'World'
     * @example default: { charset: 'abcdefg', len: 16 }
     */
    default: string | RandomString | null
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
        default: DefaultString | null
        required: boolean
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
      }
    }, string.optional())
  }
  static textarea<Required extends boolean>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    default: string | null
    required: Required
    minLength?: number | null
    maxLength?: number | null
    placeholder?: string | null
    /**
     * @description Once set, the value can never be changed.
     * @default false
     */
    immutable?: boolean
  }) {
    return new Value<AsRequired<string, Required>, never>(
      async () => {
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
      },
      asRequiredParser(string, a),
    )
  }
  static dynamicTextarea<Store = never>(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        default: string | null
        required: boolean
        minLength?: number | null
        maxLength?: number | null
        placeholder?: string | null
        disabled?: false | string
      }
    >,
  ) {
    return new Value<string | null | undefined, Store>(async (options) => {
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
    }, string.optional())
  }
  static number<Required extends boolean>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description optionally provide a default value.
     * @type { default: number | null }
     * @example default: null
     * @example default: 7
     */
    default: number | null
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
        default: number | null
        required: boolean
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
      }
    }, number.optional())
  }
  static color<Required extends boolean>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description optionally provide a default value.
     * @type { default: string | null }
     * @example default: null
     * @example default: 'ffffff'
     */
    default: string | null
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
        default: string | null
        required: boolean
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
      }
    }, string.optional())
  }
  static datetime<Required extends boolean>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description optionally provide a default value.
     * @type { default: string | null }
     * @example default: null
     * @example default: '1985-12-16 18:00:00.000'
     */
    default: string | null
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
        default: string | null
        required: boolean
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
      }
    }, string.optional())
  }
  static select<Values extends Record<string, string>>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description Determines if the field is required. If so, optionally provide a default value from the list of values.
     * @type { (keyof Values & string) | null }
     * @example default: null
     * @example default: 'radio1'
     */
    default: keyof Values & string
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
    return new Value<keyof Values & string, never>(
      () => ({
        description: null,
        warning: null,
        type: "select" as const,
        disabled: false,
        immutable: a.immutable ?? false,
        ...a,
      }),
      anyOf(
        ...Object.keys(a.values).map((x: keyof Values & string) => literal(x)),
      ),
    )
  }
  static dynamicSelect<Store = never>(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        default: string
        values: Record<string, string>
        disabled?: false | string | string[]
      }
    >,
  ) {
    return new Value<string, Store>(async (options) => {
      const a = await getA(options)
      return {
        description: null,
        warning: null,
        type: "select" as const,
        disabled: false,
        immutable: false,
        ...a,
      }
    }, string)
  }
  static multiselect<Values extends Record<string, string>>(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    /**
     * @description A simple list of which options should be checked by default.
     */
    default: (keyof Values & string)[]
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
  // static file<Store, Required extends boolean>(a: {
  //   name: string
  //   description?: string | null
  //   extensions: string[]
  //   required: Required
  // }) {
  //   const buildValue = {
  //     type: "file" as const,
  //     description: null,
  //     warning: null,
  //     ...a,
  //   }
  //   return new Value<AsRequired<FilePath, Required>, Store>(
  //     () => ({
  //       ...buildValue,
  //     }),
  //     asRequiredParser(object({ filePath: string }), a),
  //   )
  // }
  // static dynamicFile<Store>(
  //   a: LazyBuild<
  //     Store,
  //     {
  //       name: string
  //       description?: string | null
  //       warning?: string | null
  //       extensions: string[]
  //       required: boolean
  //     }
  //   >,
  // ) {
  //   return new Value<FilePath | null | undefined, Store>(
  //     async (options) => ({
  //       type: "file" as const,
  //       description: null,
  //       warning: null,
  //       ...(await a(options)),
  //     }),
  //     object({ filePath: string }).optional(),
  //   )
  // }
  static union<
    VariantValues extends {
      [K in string]: {
        name: string
        spec: InputSpec<any, Store> | InputSpec<any, never>
      }
    },
    Store,
  >(
    a: {
      name: string
      description?: string | null
      /** Presents a warning prompt before permitting the value to change. */
      warning?: string | null
      /**
       * @description Provide a default value from the list of variants.
       * @type { string }
       * @example default: 'variant1'
       */
      default: keyof VariantValues & string
      /**
       * @description Once set, the value can never be changed.
       * @default false
       */
      immutable?: boolean
    },
    aVariants: Variants<VariantValues, Store>,
  ) {
    return new Value<typeof aVariants.validator._TYPE, Store>(
      async (options) => ({
        type: "union" as const,
        description: null,
        warning: null,
        disabled: false,
        ...a,
        variants: await aVariants.build(options as any),
        immutable: a.immutable ?? false,
      }),
      aVariants.validator,
    )
  }
  static filteredUnion<
    VariantValues extends {
      [K in string]: {
        name: string
        spec: InputSpec<any, Store> | InputSpec<any, never>
      }
    },
    Store,
  >(
    getDisabledFn: LazyBuild<Store, string[] | false | string>,
    a: {
      name: string
      description?: string | null
      warning?: string | null
      default: keyof VariantValues & string
    },
    aVariants: Variants<VariantValues, Store> | Variants<VariantValues, never>,
  ) {
    return new Value<typeof aVariants.validator._TYPE, Store>(
      async (options) => ({
        type: "union" as const,
        description: null,
        warning: null,
        ...a,
        variants: await aVariants.build(options as any),
        disabled: (await getDisabledFn(options)) || false,
        immutable: false,
      }),
      aVariants.validator,
    )
  }
  static dynamicUnion<
    VariantValues extends {
      [K in string]: {
        name: string
        spec: InputSpec<any, Store> | InputSpec<any, never>
      }
    },
    Store,
  >(
    getA: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        default: keyof VariantValues & string
        disabled: string[] | false | string
      }
    >,
    aVariants: Variants<VariantValues, Store> | Variants<VariantValues, never>,
  ) {
    return new Value<typeof aVariants.validator._TYPE, Store>(
      async (options) => {
        const newValues = await getA(options)
        return {
          type: "union" as const,
          description: null,
          warning: null,
          ...newValues,
          variants: await aVariants.build(options as any),
          immutable: false,
        }
      },
      aVariants.validator,
    )
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
