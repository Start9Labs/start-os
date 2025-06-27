import { ExtractInputSpecType, InputSpec, LazyBuild } from "./inputSpec"
import { List } from "./list"
import { UnionRes, UnionResStaticValidatedAs, Variants } from "./variants"
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
} from "ts-matches"
import { DeepPartial } from "../../../types"

type AsRequired<T, Required extends boolean> = Required extends true
  ? T
  : T | null

const testForAsRequiredParser = once(
  () => object({ required: literal(true) }).test,
)
function asRequiredParser<Type, Input extends { required: boolean }>(
  parser: Parser<unknown, Type>,
  input: Input,
): Parser<unknown, AsRequired<Type, Input["required"]>> {
  if (testForAsRequiredParser()(input)) return parser as any
  return parser.nullable() as any
}

export class Value<Type extends StaticValidatedAs, StaticValidatedAs = Type> {
  protected constructor(
    public build: LazyBuild<{
      spec: ValueSpec
      validator: Parser<unknown, Type>
    }>,
    public readonly validator: Parser<unknown, StaticValidatedAs>,
  ) {}
  public _TYPE: Type = null as any as Type
  public _PARTIAL: DeepPartial<Type> = null as any as DeepPartial<Type>

  /**
   * @description Displays a boolean toggle to enable/disable
   * @example
   * ```
    toggleExample: Value.toggle({
      // required
      name: 'Toggle Example',
      default: true,
    
      // optional
      description: null,
      warning: null,
      immutable: false,
    }),
    * ```
   */
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
    const validator = boolean
    return new Value<boolean>(
      async () => ({
        spec: {
          description: null,
          warning: null,
          type: "toggle" as const,
          disabled: false,
          immutable: a.immutable ?? false,
          ...a,
        },
        validator,
      }),
      validator,
    )
  }
  static dynamicToggle(
    a: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      default: boolean
      disabled?: false | string
    }>,
  ) {
    const validator = boolean
    return new Value<boolean>(
      async (options) => ({
        spec: {
          description: null,
          warning: null,
          type: "toggle" as const,
          disabled: false,
          immutable: false,
          ...(await a(options)),
        },
        validator,
      }),
      validator,
    )
  }
  /**
   * @description Displays a text input field
   * @example
   * ```
    textExample: Value.text({
      // required
      name: 'Text Example',
      required: false,
      default: null,
    
      // optional
      description: null,
      placeholder: null,
      warning: null,
      generate: null,
      inputmode: 'text',
      masked: false,
      minLength: null,
      maxLength: null,
      patterns: [],
      immutable: false,
    }),
    * ```
    */
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
    const validator = asRequiredParser(string, a)
    return new Value<AsRequired<string, Required>>(
      async () => ({
        spec: {
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
        },
        validator,
      }),
      validator,
    )
  }
  static dynamicText<Required extends boolean>(
    getA: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      default: DefaultString | null
      required: Required
      masked?: boolean
      placeholder?: string | null
      minLength?: number | null
      maxLength?: number | null
      patterns?: Pattern[]
      inputmode?: ValueSpecText["inputmode"]
      disabled?: string | false
      generate?: null | RandomString
    }>,
  ) {
    return new Value<AsRequired<string, Required>, string | null>(
      async (options) => {
        const a = await getA(options)
        return {
          spec: {
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
          },
          validator: asRequiredParser(string, a),
        }
      },
      string.nullable(),
    )
  }
  /**
   * @description Displays a large textarea field for long form entry.
   * @example
   * ```
    textareaExample: Value.textarea({
      // required
      name: 'Textarea Example',
      required: false,
      default: null,
    
      // optional
      description: null,
      placeholder: null,
      warning: null,
      minLength: null,
      maxLength: null,
      immutable: false,
    }),
    * ```
    */
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
    const validator = asRequiredParser(string, a)
    return new Value<AsRequired<string, Required>>(async () => {
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
      return { spec: built, validator }
    }, validator)
  }
  static dynamicTextarea<Required extends boolean>(
    getA: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      default: string | null
      required: Required
      minLength?: number | null
      maxLength?: number | null
      placeholder?: string | null
      disabled?: false | string
    }>,
  ) {
    return new Value<AsRequired<string, Required>, string | null>(
      async (options) => {
        const a = await getA(options)
        return {
          spec: {
            description: null,
            warning: null,
            minLength: null,
            maxLength: null,
            placeholder: null,
            type: "textarea" as const,
            disabled: false,
            immutable: false,
            ...a,
          },
          validator: asRequiredParser(string, a),
        }
      },
      string.nullable(),
    )
  }
  /**
   * @description Displays a number input field
   * @example
   * ```
    numberExample: Value.number({
      // required
      name: 'Number Example',
      required: false,
      default: null,
      integer: true,
    
      // optional
      description: null,
      placeholder: null,
      warning: null,
      min: null,
      max: null,
      immutable: false,
      step: null,
      units: null,
    }),
    * ```
    */
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
    const validator = asRequiredParser(number, a)
    return new Value<AsRequired<number, Required>>(
      () => ({
        spec: {
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
        },
        validator,
      }),
      validator,
    )
  }
  static dynamicNumber<Required extends boolean>(
    getA: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      default: number | null
      required: Required
      min?: number | null
      max?: number | null
      step?: number | null
      integer: boolean
      units?: string | null
      placeholder?: string | null
      disabled?: false | string
    }>,
  ) {
    return new Value<AsRequired<number, Required>, number | null>(
      async (options) => {
        const a = await getA(options)
        return {
          spec: {
            type: "number" as const,
            description: null,
            warning: null,
            min: null,
            max: null,
            step: null,
            units: null,
            placeholder: null,
            disabled: false as const,
            immutable: false,
            ...a,
          },
          validator: asRequiredParser(number, a),
        }
      },
      number.nullable(),
    )
  }
  /**
   * @description Displays a browser-native color selector.
   * @example
   * ```
    colorExample: Value.color({
      // required
      name: 'Color Example',
      required: false,
      default: null,
    
      // optional
      description: null,
      warning: null,
      immutable: false,
    }),
    * ```
    */
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
    const validator = asRequiredParser(string, a)
    return new Value<AsRequired<string, Required>>(
      () => ({
        spec: {
          type: "color" as const,
          description: null,
          warning: null,
          disabled: false,
          immutable: a.immutable ?? false,
          ...a,
        },
        validator,
      }),
      validator,
    )
  }

  static dynamicColor<Required extends boolean>(
    getA: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      default: string | null
      required: Required
      disabled?: false | string
    }>,
  ) {
    return new Value<AsRequired<string, Required>, string | null>(
      async (options) => {
        const a = await getA(options)
        return {
          spec: {
            type: "color" as const,
            description: null,
            warning: null,
            disabled: false,
            immutable: false,
            ...a,
          },
          validator: asRequiredParser(string, a),
        }
      },
      string.nullable(),
    )
  }
  /**
   * @description Displays a browser-native date/time selector.
   * @example
   * ```
    datetimeExample: Value.datetime({
      // required
      name: 'Datetime Example',
      required: false,
      default: null,
    
      // optional
      description: null,
      warning: null,
      immutable: false,
      inputmode: 'datetime-local',
      min: null,
      max: null,
    }),
    * ```
    */
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
    const validator = asRequiredParser(string, a)
    return new Value<AsRequired<string, Required>>(
      () => ({
        spec: {
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
        },
        validator,
      }),
      validator,
    )
  }
  static dynamicDatetime<Required extends boolean>(
    getA: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      default: string | null
      required: Required
      inputmode?: ValueSpecDatetime["inputmode"]
      min?: string | null
      max?: string | null
      disabled?: false | string
    }>,
  ) {
    return new Value<AsRequired<string, Required>, string | null>(
      async (options) => {
        const a = await getA(options)
        return {
          spec: {
            type: "datetime" as const,
            description: null,
            warning: null,
            inputmode: "datetime-local",
            min: null,
            max: null,
            disabled: false,
            immutable: false,
            ...a,
          },
          validator: asRequiredParser(string, a),
        }
      },
      string.nullable(),
    )
  }
  /**
   * @description Displays a select modal with radio buttons, allowing for a single selection.
   * @example
   * ```
    selectExample: Value.select({
      // required
      name: 'Select Example',
      default: 'radio1',
      values: {
        radio1: 'Radio 1',
        radio2: 'Radio 2',
      },
    
      // optional
      description: null,
      warning: null,
      immutable: false,
      disabled: false,
    }),
    * ```
    */
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
    const validator = anyOf(
      ...Object.keys(a.values).map((x: keyof Values & string) => literal(x)),
    )
    return new Value<keyof Values & string>(
      () => ({
        spec: {
          description: null,
          warning: null,
          type: "select" as const,
          disabled: false,
          immutable: a.immutable ?? false,
          ...a,
        },
        validator,
      }),
      validator,
    )
  }
  static dynamicSelect<Values extends Record<string, string>>(
    getA: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      default: string
      values: Values
      disabled?: false | string | string[]
    }>,
  ) {
    return new Value<keyof Values & string, string>(async (options) => {
      const a = await getA(options)
      return {
        spec: {
          description: null,
          warning: null,
          type: "select" as const,
          disabled: false,
          immutable: false,
          ...a,
        },
        validator: anyOf(
          ...Object.keys(a.values).map((x: keyof Values & string) =>
            literal(x),
          ),
        ),
      }
    }, string)
  }
  /**
   * @description Displays a select modal with checkboxes, allowing for multiple selections.
   * @example
   * ```
    multiselectExample: Value.multiselect({
      // required
      name: 'Multiselect Example',
      values: {
        option1: 'Option 1',
        option2: 'Option 2',
      },
      default: [],
    
      // optional
      description: null,
      warning: null,
      immutable: false,
      disabled: false,
      minlength: null,
      maxLength: null,
    }),
    * ```
    */
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
    const validator = arrayOf(
      literals(...(Object.keys(a.values) as any as [keyof Values & string])),
    )
    return new Value<(keyof Values & string)[]>(
      () => ({
        spec: {
          type: "multiselect" as const,
          minLength: null,
          maxLength: null,
          warning: null,
          description: null,
          disabled: false,
          immutable: a.immutable ?? false,
          ...a,
        },
        validator,
      }),
      validator,
    )
  }
  static dynamicMultiselect<Values extends Record<string, string>>(
    getA: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      default: string[]
      values: Values
      minLength?: number | null
      maxLength?: number | null
      disabled?: false | string | string[]
    }>,
  ) {
    return new Value<(keyof Values & string)[], string[]>(async (options) => {
      const a = await getA(options)
      return {
        spec: {
          type: "multiselect" as const,
          minLength: null,
          maxLength: null,
          warning: null,
          description: null,
          disabled: false,
          immutable: false,
          ...a,
        },
        validator: arrayOf(
          literals(
            ...(Object.keys(a.values) as any as [keyof Values & string]),
          ),
        ),
      }
    }, arrayOf(string))
  }
  /**
   * @description Display a collapsable grouping of additional fields, a "sub form". The second value is the inputSpec spec for the sub form.
   * @example
   * ```
    objectExample: Value.object(
      {
        // required
        name: 'Object Example',
    
        // optional
        description: null,
        warning: null,
      },
      InputSpec.of({}),
    ),
    * ```
    */
  static object<
    Type extends StaticValidatedAs,
    StaticValidatedAs extends Record<string, any>,
  >(
    a: {
      name: string
      description?: string | null
    },
    spec: InputSpec<Type, StaticValidatedAs>,
  ) {
    return new Value<Type, StaticValidatedAs>(async (options) => {
      const built = await spec.build(options as any)
      return {
        spec: {
          type: "object" as const,
          description: null,
          warning: null,
          ...a,
          spec: built.spec,
        },
        validator: built.validator,
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
  //   return new Value<FilePath | null, Store>(
  //     async (options) => ({
  //       type: "file" as const,
  //       description: null,
  //       warning: null,
  //       ...(await a(options)),
  //     }),
  //     object({ filePath: string }).nullable(),
  //   )
  // }
  /**
   * @description Displays a dropdown, allowing for a single selection. Depending on the selection, a different object ("sub form") is presented.
   * @example
   * ```
    unionExample: Value.union(
      {
        // required
        name: 'Union Example',
        default: 'option1',
    
        // optional
        description: null,
        warning: null,
        disabled: false,
        immutable: false,
      },
      Variants.of({
        option1: {
          name: 'Option 1',
          spec: InputSpec.of({}),
        },
        option2: {
          name: 'Option 2',
          spec: InputSpec.of({}),
        },
      }),
    ),
    * ```
    */
  static union<
    VariantValues extends {
      [K in string]: {
        name: string
        spec: InputSpec<any>
      }
    },
  >(a: {
    name: string
    description?: string | null
    /** Presents a warning prompt before permitting the value to change. */
    warning?: string | null
    variants: Variants<VariantValues>
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
  }) {
    return new Value<
      typeof a.variants._TYPE,
      typeof a.variants.validator._TYPE
    >(async (options) => {
      const built = await a.variants.build(options as any)
      return {
        spec: {
          type: "union" as const,
          description: null,
          warning: null,
          disabled: false,
          ...a,
          variants: built.spec,
          immutable: a.immutable ?? false,
        },
        validator: built.validator,
      }
    }, a.variants.validator)
  }
  static dynamicUnion<
    VariantValues extends {
      [K in string]: {
        name: string
        spec: InputSpec<any>
      }
    },
  >(
    getA: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      variants: Variants<VariantValues>
      default: keyof VariantValues & string
      disabled: string[] | false | string
    }>,
    staticVariants: Variants<VariantValues>,
  ) {
    return new Value<
      UnionRes<VariantValues>,
      typeof staticVariants.validator._TYPE
    >(async (options) => {
      const newValues = await getA(options)
      const built = await newValues.variants.build(options as any)
      return {
        spec: {
          type: "union" as const,
          description: null,
          warning: null,
          ...newValues,
          variants: built.spec,
          immutable: false,
        },
        validator: built.validator,
      }
    }, staticVariants.validator)
  }
  /**
   * @description Presents an interface to add/remove/edit items in a list.
   * @example
   * In this example, we create a list of text inputs.
   * 
   * ```
    listExampleText: Value.list(
      List.text(
        {
          // required
          name: 'Text List',

          // optional
          description: null,
          warning: null,
          default: [],
          minLength: null,
          maxLength: null,
        },
        {
          // required
          patterns: [],

          // optional
          placeholder: null,
          generate: null,
          inputmode: 'url',
          masked: false,
          minLength: null,
          maxLength: null,
        },
      ),
    ),
   * ```
   * @example
   * In this example, we create a list of objects.
   * 
   * ```
    listExampleObject: Value.list(
      List.obj(
        {
          // required
          name: 'Object List',

          // optional
          description: null,
          warning: null,
          default: [],
          minLength: null,
          maxLength: null,
        },
        {
          // required
          spec: InputSpec.of({}),

          // optional
          displayAs: null,
          uniqueBy: null,
        },
      ),
    ),
   * ```
   */
  static list<Type>(a: List<Type>) {
    return new Value<Type>((options) => a.build(options), a.validator)
  }

  /**
   * @description Provides a way to define a hidden field with a static value. Useful for tracking 
   * @example
   * ```
    hiddenExample: Value.hidden(),
   * ```
   */
  static hidden<T>(): Value<T, unknown>
  static hidden<T>(parser: Parser<unknown, T>): Value<T>
  static hidden<T>(parser: Parser<unknown, T> = any) {
    return new Value<T, typeof parser._TYPE>(async () => {
      return {
        spec: {
          type: "hidden" as const,
        } as ValueSpecHidden,
        validator: parser,
      }
    }, parser)
  }

  /**
   * @description Provides a way to define a hidden field with a static value. Useful for tracking 
   * @example
   * ```
    hiddenExample: Value.hidden(),
   * ```
   */
  static dynamicHidden<T>(getParser: LazyBuild<Parser<unknown, T>>) {
    return new Value<T, unknown>(async (options) => {
      const validator = await getParser(options)
      return {
        spec: {
          type: "hidden" as const,
        } as ValueSpecHidden,
        validator,
      }
    }, any)
  }

  map<U>(fn: (value: StaticValidatedAs) => U): Value<U> {
    return new Value(async (effects) => {
      const built = await this.build(effects)
      return {
        spec: built.spec,
        validator: built.validator.map(fn),
      }
    }, this.validator.map(fn))
  }
}
