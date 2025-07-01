import * as fs from "fs"

// https://stackoverflow.com/questions/2970525/converting-any-string-into-camel-case
export function camelCase(value: string) {
  return value
    .replace(/([\(\)\[\]])/g, "")
    .replace(/^([A-Z])|[\s-_](\w)/g, function (match, p1, p2, offset) {
      if (p2) return p2.toUpperCase()
      return p1.toLowerCase()
    })
}

export async function oldSpecToBuilder(
  file: string,
  inputData: Promise<any> | any,
  options?: Parameters<typeof makeFileContentFromOld>[1],
) {
  await fs.writeFile(
    file,
    await makeFileContentFromOld(inputData, options),
    (err) => console.error(err),
  )
}

function isString(x: unknown): x is string {
  return typeof x === "string"
}

export default async function makeFileContentFromOld(
  inputData: Promise<any> | any,
  { StartSdk = "start-sdk", nested = true } = {},
) {
  const outputLines: string[] = []
  outputLines.push(`
import { sdk } from "${StartSdk}"
const {InputSpec, List, Value, Variants} = sdk
`)
  const data = await inputData

  const namedConsts = new Set(["InputSpec", "Value", "List"])
  const inputSpecName = newConst("inputSpecSpec", convertInputSpec(data))
  outputLines.push(`export type InputSpecSpec = typeof ${inputSpecName}._TYPE;`)

  return outputLines.join("\n")

  function newConst(key: string, data: string, type?: string) {
    const variableName = getNextConstName(camelCase(key))
    outputLines.push(
      `export const ${variableName}${!type ? "" : `: ${type}`} = ${data};`,
    )
    return variableName
  }
  function maybeNewConst(key: string, data: string) {
    if (nested) return data
    return newConst(key, data)
  }
  function convertInputSpecInner(data: any) {
    let answer = "{"
    for (const [key, value] of Object.entries(data)) {
      const variableName = maybeNewConst(key, convertValueSpec(value))

      answer += `${JSON.stringify(key)}: ${variableName},`
    }
    return `${answer}}`
  }

  function convertInputSpec(data: any) {
    return `InputSpec.of(${convertInputSpecInner(data)})`
  }
  function convertValueSpec(value: any): string {
    switch (value.type) {
      case "string": {
        if (value.textarea) {
          return `${rangeToTodoComment(
            value?.range,
          )}Value.textarea(${JSON.stringify(
            {
              name: value.name || null,
              description: value.description || null,
              warning: value.warning || null,
              required: !(value.nullable || false),
              default: value.default,
              placeholder: value.placeholder || null,
              maxLength: null,
              minLength: null,
            },
            null,
            2,
          )})`
        }
        return `${rangeToTodoComment(value?.range)}Value.text(${JSON.stringify(
          {
            name: value.name || null,
            default: value.default || null,
            required: !value.nullable,
            description: value.description || null,
            warning: value.warning || null,
            masked: value.masked || false,
            placeholder: value.placeholder || null,
            inputmode: "text",
            patterns: value.pattern
              ? [
                  {
                    regex: value.pattern,
                    description: value["pattern-description"],
                  },
                ]
              : [],
            minLength: null,
            maxLength: null,
          },
          null,
          2,
        )})`
      }
      case "number": {
        return `${rangeToTodoComment(
          value?.range,
        )}Value.number(${JSON.stringify(
          {
            name: value.name || null,
            description: value.description || null,
            warning: value.warning || null,
            default: value.default || null,
            required: !value.nullable,
            min: null,
            max: null,
            step: null,
            integer: value.integral || false,
            units: value.units || null,
            placeholder: value.placeholder || null,
          },
          null,
          2,
        )})`
      }
      case "boolean": {
        return `Value.toggle(${JSON.stringify(
          {
            name: value.name || null,
            default: value.default || false,
            description: value.description || null,
            warning: value.warning || null,
          },
          null,
          2,
        )})`
      }
      case "enum": {
        const allValueNames = new Set([
          ...(value?.["values"] || []),
          ...Object.keys(value?.["value-names"] || {}),
        ])
        const values = Object.fromEntries(
          Array.from(allValueNames)
            .filter(isString)
            .map((key) => [key, value?.spec?.["value-names"]?.[key] || key]),
        )
        return `Value.select(${JSON.stringify(
          {
            name: value.name || null,
            description: value.description || null,
            warning: value.warning || null,
            default: value.default,
            values,
          },
          null,
          2,
        )} as const)`
      }
      case "object": {
        const specName = maybeNewConst(
          value.name + "_spec",
          convertInputSpec(value.spec),
        )
        return `Value.object({
        name: ${JSON.stringify(value.name || null)},
        description: ${JSON.stringify(value.description || null)},
      }, ${specName})`
      }
      case "union": {
        const variants = maybeNewConst(
          value.name + "_variants",
          convertVariants(value.variants, value.tag["variant-names"] || {}),
        )

        return `Value.union({
        name: ${JSON.stringify(value.name || null)},
        description: ${JSON.stringify(value.tag.description || null)},
        warning: ${JSON.stringify(value.tag.warning || null)},
        default: ${JSON.stringify(value.default)},
        variants: ${variants},
      })`
      }
      case "list": {
        if (value.subtype === "enum") {
          const allValueNames = new Set([
            ...(value?.spec?.["values"] || []),
            ...Object.keys(value?.spec?.["value-names"] || {}),
          ])
          const values = Object.fromEntries(
            Array.from(allValueNames)
              .filter(isString)
              .map((key: string) => [
                key,
                value?.spec?.["value-names"]?.[key] ?? key,
              ]),
          )
          return `Value.multiselect(${JSON.stringify(
            {
              name: value.name || null,
              minLength: null,
              maxLength: null,
              default: value.default ?? null,
              description: value.description || null,
              warning: value.warning || null,
              values,
            },
            null,
            2,
          )})`
        }
        const list = maybeNewConst(value.name + "_list", convertList(value))
        return `Value.list(${list})`
      }
      case "pointer": {
        return `/* TODO deal with point removed point "${value.name}" */null as any`
      }
    }
    throw Error(`Unknown type "${value.type}"`)
  }

  function convertList(value: any) {
    switch (value.subtype) {
      case "string": {
        return `${rangeToTodoComment(value?.range)}List.text(${JSON.stringify(
          {
            name: value.name || null,
            minLength: null,
            maxLength: null,
            default: value.default || null,
            description: value.description || null,
            warning: value.warning || null,
          },
          null,
          2,
        )}, ${JSON.stringify({
          masked: value?.spec?.masked || false,
          placeholder: value?.spec?.placeholder || null,
          patterns: value?.spec?.pattern
            ? [
                {
                  regex: value.spec.pattern,
                  description: value?.spec?.["pattern-description"],
                },
              ]
            : [],
          minLength: null,
          maxLength: null,
        })})`
      }
      // case "number": {
      //   return `${rangeToTodoComment(value?.range)}List.number(${JSON.stringify(
      //     {
      //       name: value.name || null,
      //       minLength: null,
      //       maxLength: null,
      //       default: value.default || null,
      //       description: value.description || null,
      //       warning: value.warning || null,
      //     },
      //     null,
      //     2,
      //   )}, ${JSON.stringify({
      //     integer: value?.spec?.integral || false,
      //     min: null,
      //     max: null,
      //     units: value?.spec?.units || null,
      //     placeholder: value?.spec?.placeholder || null,
      //   })})`
      // }
      case "enum": {
        return "/* error!! list.enum */"
      }
      case "object": {
        const specName = maybeNewConst(
          value.name + "_spec",
          convertInputSpec(value.spec.spec),
        )
        return `${rangeToTodoComment(value?.range)}List.obj({
          name: ${JSON.stringify(value.name || null)},
          minLength: ${JSON.stringify(null)},
          maxLength: ${JSON.stringify(null)},
          default: ${JSON.stringify(value.default || null)},
          description: ${JSON.stringify(value.description || null)},
        }, {
          spec: ${specName},
          displayAs: ${JSON.stringify(value?.spec?.["display-as"] || null)},
          uniqueBy: ${JSON.stringify(value?.spec?.["unique-by"] || null)},
        })`
      }
      case "union": {
        const variants = maybeNewConst(
          value.name + "_variants",
          convertVariants(
            value.spec.variants,
            value.spec["variant-names"] || {},
          ),
        )
        const unionValueName = maybeNewConst(
          value.name + "_union",
          `${rangeToTodoComment(value?.range)}
          Value.union({
            name: ${JSON.stringify(value?.spec?.tag?.name || null)},
            description: ${JSON.stringify(
              value?.spec?.tag?.description || null,
            )},
            warning: ${JSON.stringify(value?.spec?.tag?.warning || null)},
            default: ${JSON.stringify(value?.spec?.default || null)},
            variants: ${variants},
          })
        `,
        )
        const listInputSpec = maybeNewConst(
          value.name + "_list_inputSpec",
          `
          InputSpec.of({
            "union": ${unionValueName}
          })
        `,
        )
        return `${rangeToTodoComment(value?.range)}List.obj({
          name:${JSON.stringify(value.name || null)},
          minLength:${JSON.stringify(null)},
          maxLength:${JSON.stringify(null)},
          default: [],
          description: ${JSON.stringify(value.description || null)},
          warning: ${JSON.stringify(value.warning || null)},
        }, {
          spec: ${listInputSpec},
          displayAs: ${JSON.stringify(value?.spec?.["display-as"] || null)},
          uniqueBy: ${JSON.stringify(value?.spec?.["unique-by"] || null)},
        })`
      }
    }
    throw new Error(`Unknown subtype "${value.subtype}"`)
  }

  function convertVariants(
    variants: Record<string, unknown>,
    variantNames: Record<string, string>,
  ): string {
    let answer = "Variants.of({"
    for (const [key, value] of Object.entries(variants)) {
      const variantSpec = maybeNewConst(key, convertInputSpec(value))
      answer += `"${key}": {name: "${
        variantNames[key] || key
      }", spec: ${variantSpec}},`
    }
    return `${answer}})`
  }

  function getNextConstName(name: string, i = 0): string {
    const newName = !i ? name : name + i
    if (namedConsts.has(newName)) {
      return getNextConstName(name, i + 1)
    }
    namedConsts.add(newName)
    return newName
  }
}

function rangeToTodoComment(range: string | undefined) {
  if (!range) return ""
  return `/* TODO: Convert range for this value (${range})*/`
}

// oldSpecToBuilder(
//   "./inputSpec.ts",
//   // Put inputSpec here
//   {},
// )
