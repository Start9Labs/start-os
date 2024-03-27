import { Config, LazyBuild, LazyBuildOptions } from "./config"
import { List } from "./list"
import { Variants } from "./variants"
import {
  FilePath,
  Pattern,
  RandomString,
  ValueSpec,
  ValueSpecDatetime,
  ValueSpecText,
  ValueSpecTextarea,
} from "../configTypes"
import { DefaultString } from "../configTypes"
import { _ } from "../../util"
import {
  Parser,
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
import { once } from "../../util/once"

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

type InputAsRequired<A, Type> = A extends
  | { required: { default: any } | never }
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

/**
 * A value is going to be part of the form in the FE of the OS.
 * Something like a boolean, a string, a number, etc.
 * in the fe it will ask for the name of value, and use the rest of the value to determine how to render it.
 * While writing with a value, you will start with `Value.` then let the IDE suggest the rest.
 * for things like string, the options are going to be in {}.
 * Keep an eye out for another config builder types as params.
 * Note, usually this is going to be used in a `Config` {@link Config} builder.
 ```ts
const username = Value.string({
  name: "Username",
  default: "bitcoin",
  description: "The username for connecting to Bitcoin over RPC.",
  warning: null,
  required: true,
  masked: true,
  placeholder: null,
  pattern: "^[a-zA-Z0-9_]+$",
  patternDescription: "Must be alphanumeric (can contain underscore).",
});
 ```
 */
export class Value<Type, Store> {
  protected constructor(
    public build: LazyBuild<Store, ValueSpec>,
    public validator: Parser<unknown, Type>,
  ) {}
  static toggle(a: {
    name: string
    description?: string | null
    warning?: string | null
    default: boolean
    /**  Immutable means it can only be configed at the first config then never again 
    Default is false */
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
    warning?: string | null
    required: Required

    /** Default = false */
    masked?: boolean
    placeholder?: string | null
    minLength?: number | null
    maxLength?: number | null
    patterns?: Pattern[]
    /** Default = 'text' */
    inputmode?: ValueSpecText["inputmode"]
    /**  Immutable means it can only be configured at the first config then never again
     * Default is false
     */
    immutable?: boolean
    generate?: null | RandomString
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

        /** Default = false */
        masked?: boolean
        placeholder?: string | null
        minLength?: number | null
        maxLength?: number | null
        patterns?: Pattern[]
        /** Default = 'text' */
        inputmode?: ValueSpecText["inputmode"]
        disabled?: string | false
        /**  Immutable means it can only be configured at the first config then never again
         * Default is false
         */
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
    warning?: string | null
    required: boolean
    minLength?: number | null
    maxLength?: number | null
    placeholder?: string | null
    /**  Immutable means it can only be configed at the first config then never again 
    Default is false */
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
    warning?: string | null
    required: Required
    min?: number | null
    max?: number | null
    /** Default = '1' */
    step?: number | null
    integer: boolean
    units?: string | null
    placeholder?: string | null
    /**  Immutable means it can only be configed at the first config then never again 
    Default is false */
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
        /** Default = '1' */
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
    warning?: string | null
    required: Required
    /**  Immutable means it can only be configed at the first config then never again 
    Default is false */
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
    warning?: string | null
    required: Required
    /** Default = 'datetime-local' */
    inputmode?: ValueSpecDatetime["inputmode"]
    min?: string | null
    max?: string | null
    /**  Immutable means it can only be configed at the first config then never again 
    Default is false */
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
        /** Default = 'datetime-local' */
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
    B extends Record<string, string>,
  >(a: {
    name: string
    description?: string | null
    warning?: string | null
    required: Required
    values: B
    /**
     * Disabled:  false means that there is nothing disabled, good to modify
     *           string means that this is the message displayed and the whole thing is disabled
     *           string[] means that the options are disabled
     */
    disabled?: false | string | (string & keyof B)[]
    /**  Immutable means it can only be configed at the first config then never again 
    Default is false */
    immutable?: boolean
  }) {
    return new Value<AsRequired<keyof B, Required>, never>(
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
          ...Object.keys(a.values).map((x: keyof B & string) => literal(x)),
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
        /**
         * Disabled:  false means that there is nothing disabled, good to modify
         *           string means that this is the message displayed and the whole thing is disabled
         *           string[] means that the options are disabled
         */
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
    warning?: string | null
    default: string[]
    values: Values
    minLength?: number | null
    maxLength?: number | null
    /**  Immutable means it can only be configed at the first config then never again 
    Default is false */
    immutable?: boolean
    /**
     * Disabled:  false means that there is nothing disabled, good to modify
     *           string means that this is the message displayed and the whole thing is disabled
     *           string[] means that the options are disabled
     */
    disabled?: false | string | (string & keyof Values)[]
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
        /**
         * Disabled:  false means that there is nothing disabled, good to modify
         *           string means that this is the message displayed and the whole thing is disabled
         *           string[] means that the options are disabled
         */
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
      warning?: string | null
    },
    spec: Config<Type, Store>,
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
  static file<Required extends RequiredDefault<string>, Store>(a: {
    name: string
    description?: string | null
    warning?: string | null
    extensions: string[]
    required: Required
  }) {
    const buildValue = {
      type: "file" as const,
      description: null,
      warning: null,
      ...a,
    }
    return new Value<AsRequired<FilePath, Required>, Store>(
      () => ({
        ...buildValue,

        ...requiredLikeToAbove(a.required),
      }),
      asRequiredParser(object({ filePath: string }), a),
    )
  }
  static dynamicFile<Required extends boolean, Store>(
    a: LazyBuild<
      Store,
      {
        name: string
        description?: string | null
        warning?: string | null
        extensions: string[]
        required: Required
      }
    >,
  ) {
    return new Value<string | null | undefined, Store>(
      async (options) => ({
        type: "file" as const,
        description: null,
        warning: null,
        ...(await a(options)),
      }),
      string.optional(),
    )
  }
  static union<Required extends RequiredDefault<string>, Type, Store>(
    a: {
      name: string
      description?: string | null
      warning?: string | null
      required: Required
      /**  Immutable means it can only be configed at the first config then never again 
      Default is false */
      immutable?: boolean
      /**
       * Disabled:  false means that there is nothing disabled, good to modify
       *           string means that this is the message displayed and the whole thing is disabled
       *           string[] means that the options are disabled
       */
      disabled?: false | string | string[]
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
        disabled: string[] | false | string
        name: string
        description?: string | null
        warning?: string | null
        required: Required
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

  /**
   * Use this during the times that the input needs a more specific type.
   * Used in types that the value/ variant/ list/ config is constructed somewhere else.
  ```ts
  const a = Config.text({
    name: "a",
    required: false,
  })

  return Config.of<Store>()({
    myValue: a.withStore(),
  })
  ```
   */
  withStore<NewStore extends Store extends never ? any : Store>() {
    return this as any as Value<Type, NewStore>
  }
}
