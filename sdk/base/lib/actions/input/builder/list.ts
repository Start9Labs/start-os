import { InputSpec, LazyBuild } from "./inputSpec"
import {
  ListValueSpecText,
  Pattern,
  RandomString,
  UniqueBy,
  ValueSpecList,
  ValueSpecListOf,
} from "../inputSpecTypes"
import { Parser, arrayOf, string } from "ts-matches"

export class List<Type> {
  private constructor(
    public build: LazyBuild<ValueSpecList>,
    public validator: Parser<unknown, Type>,
  ) {}

  static text(
    a: {
      name: string
      description?: string | null
      warning?: string | null
      default?: string[]
      minLength?: number | null
      maxLength?: number | null
    },
    aSpec: {
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
      inputmode?: ListValueSpecText["inputmode"]
      /**
       * @description Displays a button that will generate a random string according to the provided charset and len attributes.
       */
      generate?: null | RandomString
    },
  ) {
    return new List<string[]>(() => {
      const spec = {
        type: "text" as const,
        placeholder: null,
        minLength: null,
        maxLength: null,
        masked: false,
        inputmode: "text" as const,
        generate: null,
        patterns: aSpec.patterns || [],
        ...aSpec,
      }
      const built: ValueSpecListOf<"text"> = {
        description: null,
        warning: null,
        default: [],
        type: "list" as const,
        minLength: null,
        maxLength: null,
        disabled: false,
        ...a,
        spec,
      }
      return built
    }, arrayOf(string))
  }

  static dynamicText(
    getA: LazyBuild<{
      name: string
      description?: string | null
      warning?: string | null
      default?: string[]
      minLength?: number | null
      maxLength?: number | null
      disabled?: false | string
      generate?: null | RandomString
      spec: {
        masked?: boolean
        placeholder?: string | null
        minLength?: number | null
        maxLength?: number | null
        patterns?: Pattern[]
        inputmode?: ListValueSpecText["inputmode"]
      }
    }>,
  ) {
    return new List<string[]>(async (options) => {
      const { spec: aSpec, ...a } = await getA(options)
      const spec = {
        type: "text" as const,
        placeholder: null,
        minLength: null,
        maxLength: null,
        masked: false,
        inputmode: "text" as const,
        generate: null,
        patterns: aSpec.patterns || [],
        ...aSpec,
      }
      const built: ValueSpecListOf<"text"> = {
        description: null,
        warning: null,
        default: [],
        type: "list" as const,
        minLength: null,
        maxLength: null,
        disabled: false,
        ...a,
        spec,
      }
      return built
    }, arrayOf(string))
  }

  static obj<Type extends Record<string, any>>(
    a: {
      name: string
      description?: string | null
      warning?: string | null
      default?: []
      minLength?: number | null
      maxLength?: number | null
    },
    aSpec: {
      spec: InputSpec<Type>
      displayAs?: null | string
      uniqueBy?: null | UniqueBy
    },
  ) {
    return new List<Type[]>(async (options) => {
      const { spec: previousSpecSpec, ...restSpec } = aSpec
      const specSpec = await previousSpecSpec.build(options)
      const spec = {
        type: "object" as const,
        displayAs: null,
        uniqueBy: null,
        ...restSpec,
        spec: specSpec,
      }
      const value = {
        spec,
        default: [],
        ...a,
      }
      return {
        description: null,
        warning: null,
        minLength: null,
        maxLength: null,
        type: "list" as const,
        disabled: false,
        ...value,
      }
    }, arrayOf(aSpec.spec.validator))
  }
}
