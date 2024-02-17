import { testOutput } from "./output.test"
import { Config } from "../config/builder/config"
import { List } from "../config/builder/list"
import { Value } from "../config/builder/value"
import { Variants } from "../config/builder/variants"
import { ValueSpec } from "../config/configTypes"

describe("builder tests", () => {
  test("text", async () => {
    const bitcoinPropertiesBuilt: {
      "peer-tor-address": ValueSpec
    } = await Config.of({
      "peer-tor-address": Value.text({
        name: "Peer tor address",
        description: "The Tor address of the peer interface",
        required: { default: null },
      }),
    }).build({} as any)
    expect(bitcoinPropertiesBuilt).toMatchObject({
      "peer-tor-address": {
        type: "text",
        description: "The Tor address of the peer interface",
        warning: null,
        masked: false,
        placeholder: null,
        minLength: null,
        maxLength: null,
        patterns: [],
        disabled: false,
        inputmode: "text",
        name: "Peer tor address",
        required: true,
        default: null,
      },
    })
  })
})

describe("values", () => {
  test("toggle", async () => {
    const value = Value.toggle({
      name: "Testing",
      description: null,
      warning: null,
      default: false,
    })
    const validator = value.validator
    validator.unsafeCast(false)
    testOutput<typeof validator._TYPE, boolean>()(null)
  })
  test("text", async () => {
    const value = Value.text({
      name: "Testing",
      required: { default: null },
    })
    const validator = value.validator
    const rawIs = await value.build({} as any)
    validator.unsafeCast("test text")
    expect(() => validator.unsafeCast(null)).toThrowError()
    testOutput<typeof validator._TYPE, string>()(null)
  })
  test("text with default", async () => {
    const value = Value.text({
      name: "Testing",
      required: { default: "this is a default value" },
    })
    const validator = value.validator
    const rawIs = await value.build({} as any)
    validator.unsafeCast("test text")
    expect(() => validator.unsafeCast(null)).toThrowError()
    testOutput<typeof validator._TYPE, string>()(null)
  })
  test("optional text", async () => {
    const value = Value.text({
      name: "Testing",
      required: false,
    })
    const validator = value.validator
    const rawIs = await value.build({} as any)
    validator.unsafeCast("test text")
    validator.unsafeCast(null)
    testOutput<typeof validator._TYPE, string | null | undefined>()(null)
  })
  test("color", async () => {
    const value = Value.color({
      name: "Testing",
      required: false,
      description: null,
      warning: null,
    })
    const validator = value.validator
    validator.unsafeCast("#000000")
    testOutput<typeof validator._TYPE, string | null | undefined>()(null)
  })
  test("datetime", async () => {
    const value = Value.datetime({
      name: "Testing",
      required: { default: null },
      description: null,
      warning: null,
      inputmode: "date",
      min: null,
      max: null,
    })
    const validator = value.validator
    validator.unsafeCast("2021-01-01")
    testOutput<typeof validator._TYPE, string>()(null)
  })
  test("optional datetime", async () => {
    const value = Value.datetime({
      name: "Testing",
      required: false,
      description: null,
      warning: null,
      inputmode: "date",
      min: null,
      max: null,
    })
    const validator = value.validator
    validator.unsafeCast("2021-01-01")
    testOutput<typeof validator._TYPE, string | null | undefined>()(null)
  })
  test("textarea", async () => {
    const value = Value.textarea({
      name: "Testing",
      required: false,
      description: null,
      warning: null,
      minLength: null,
      maxLength: null,
      placeholder: null,
    })
    const validator = value.validator
    validator.unsafeCast("test text")
    testOutput<typeof validator._TYPE, string>()(null)
  })
  test("number", async () => {
    const value = Value.number({
      name: "Testing",
      required: { default: null },
      integer: false,
      description: null,
      warning: null,
      min: null,
      max: null,
      step: null,
      units: null,
      placeholder: null,
    })
    const validator = value.validator
    validator.unsafeCast(2)
    testOutput<typeof validator._TYPE, number>()(null)
  })
  test("optional number", async () => {
    const value = Value.number({
      name: "Testing",
      required: false,
      integer: false,
      description: null,
      warning: null,
      min: null,
      max: null,
      step: null,
      units: null,
      placeholder: null,
    })
    const validator = value.validator
    validator.unsafeCast(2)
    testOutput<typeof validator._TYPE, number | null | undefined>()(null)
  })
  test("select", async () => {
    const value = Value.select({
      name: "Testing",
      required: { default: null },
      values: {
        a: "A",
        b: "B",
      },
      description: null,
      warning: null,
    })
    const validator = value.validator
    validator.unsafeCast("a")
    validator.unsafeCast("b")
    expect(() => validator.unsafeCast("c")).toThrowError()
    testOutput<typeof validator._TYPE, "a" | "b">()(null)
  })
  test("nullable select", async () => {
    const value = Value.select({
      name: "Testing",
      required: false,
      values: {
        a: "A",
        b: "B",
      },
      description: null,
      warning: null,
    })
    const validator = value.validator
    validator.unsafeCast("a")
    validator.unsafeCast("b")
    validator.unsafeCast(null)
    testOutput<typeof validator._TYPE, "a" | "b" | null | undefined>()(null)
  })
  test("multiselect", async () => {
    const value = Value.multiselect({
      name: "Testing",
      values: {
        a: "A",
        b: "B",
      },
      default: [],
      description: null,
      warning: null,
      minLength: null,
      maxLength: null,
    })
    const validator = value.validator
    validator.unsafeCast([])
    validator.unsafeCast(["a", "b"])

    expect(() => validator.unsafeCast(["e"])).toThrowError()
    expect(() => validator.unsafeCast([4])).toThrowError()
    testOutput<typeof validator._TYPE, Array<"a" | "b">>()(null)
  })
  test("object", async () => {
    const value = Value.object(
      {
        name: "Testing",
        description: null,
        warning: null,
      },
      Config.of({
        a: Value.toggle({
          name: "test",
          description: null,
          warning: null,
          default: false,
        }),
      }),
    )
    const validator = value.validator
    validator.unsafeCast({ a: true })
    testOutput<typeof validator._TYPE, { a: boolean }>()(null)
  })
  test("union", async () => {
    const value = Value.union(
      {
        name: "Testing",
        required: { default: null },
        description: null,
        warning: null,
      },
      Variants.of({
        a: {
          name: "a",
          spec: Config.of({
            b: Value.toggle({
              name: "b",
              description: null,
              warning: null,
              default: false,
            }),
          }),
        },
      }),
    )
    const validator = value.validator
    validator.unsafeCast({ unionSelectKey: "a", unionValueKey: { b: false } })
    type Test = typeof validator._TYPE
    testOutput<Test, { unionSelectKey: "a"; unionValueKey: { b: boolean } }>()(
      null,
    )
  })
  test("list", async () => {
    const value = Value.list(
      List.number(
        {
          name: "test",
        },
        {
          integer: false,
        },
      ),
    )
    const validator = value.validator
    validator.unsafeCast([1, 2, 3])
    testOutput<typeof validator._TYPE, number[]>()(null)
  })

  describe("dynamic", () => {
    const fakeOptions = {
      config: "config",
      effects: "effects",
      utils: "utils",
    } as any
    test("toggle", async () => {
      const value = Value.dynamicToggle(async () => ({
        name: "Testing",
        description: null,
        warning: null,
        default: false,
      }))
      const validator = value.validator
      validator.unsafeCast(false)
      expect(() => validator.unsafeCast(null)).toThrowError()
      testOutput<typeof validator._TYPE, boolean>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        description: null,
        warning: null,
        default: false,
      })
    })
    test("text", async () => {
      const value = Value.dynamicText(async () => ({
        name: "Testing",
        required: { default: null },
      }))
      const validator = value.validator
      const rawIs = await value.build({} as any)
      validator.unsafeCast("test text")
      validator.unsafeCast(null)
      testOutput<typeof validator._TYPE, string | null | undefined>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        required: true,
        default: null,
      })
    })
    test("text with default", async () => {
      const value = Value.dynamicText(async () => ({
        name: "Testing",
        required: { default: "this is a default value" },
      }))
      const validator = value.validator
      validator.unsafeCast("test text")
      validator.unsafeCast(null)
      testOutput<typeof validator._TYPE, string | null | undefined>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        required: true,
        default: "this is a default value",
      })
    })
    test("optional text", async () => {
      const value = Value.dynamicText(async () => ({
        name: "Testing",
        required: false,
      }))
      const validator = value.validator
      const rawIs = await value.build({} as any)
      validator.unsafeCast("test text")
      validator.unsafeCast(null)
      testOutput<typeof validator._TYPE, string | null | undefined>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        required: false,
        default: null,
      })
    })
    test("color", async () => {
      const value = Value.dynamicColor(async () => ({
        name: "Testing",
        required: false,
        description: null,
        warning: null,
      }))
      const validator = value.validator
      validator.unsafeCast("#000000")
      validator.unsafeCast(null)
      testOutput<typeof validator._TYPE, string | null | undefined>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        required: false,
        default: null,
        description: null,
        warning: null,
      })
    })
    test("datetime", async () => {
      const value = Value.dynamicDatetime<{ test: "a" }>(async ({ utils }) => {
        ;async () => {
          ;(await utils.store.getOwn("/test").once()) satisfies "a"
        }

        return {
          name: "Testing",
          required: { default: null },
          inputmode: "date",
        }
      })
      const validator = value.validator
      validator.unsafeCast("2021-01-01")
      validator.unsafeCast(null)
      testOutput<typeof validator._TYPE, string | null | undefined>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        required: true,
        default: null,
        description: null,
        warning: null,
        inputmode: "date",
      })
    })
    test("textarea", async () => {
      const value = Value.dynamicTextarea(async () => ({
        name: "Testing",
        required: false,
        description: null,
        warning: null,
        minLength: null,
        maxLength: null,
        placeholder: null,
      }))
      const validator = value.validator
      validator.unsafeCast("test text")
      expect(() => validator.unsafeCast(null)).toThrowError()
      testOutput<typeof validator._TYPE, string>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        required: false,
      })
    })
    test("number", async () => {
      const value = Value.dynamicNumber(() => ({
        name: "Testing",
        required: { default: null },
        integer: false,
        description: null,
        warning: null,
        min: null,
        max: null,
        step: null,
        units: null,
        placeholder: null,
      }))
      const validator = value.validator
      validator.unsafeCast(2)
      validator.unsafeCast(null)
      expect(() => validator.unsafeCast("null")).toThrowError()
      testOutput<typeof validator._TYPE, number | null | undefined>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        required: true,
      })
    })
    test("select", async () => {
      const value = Value.dynamicSelect(() => ({
        name: "Testing",
        required: { default: null },
        values: {
          a: "A",
          b: "B",
        },
        description: null,
        warning: null,
      }))
      const validator = value.validator
      validator.unsafeCast("a")
      validator.unsafeCast("b")
      validator.unsafeCast("c")
      validator.unsafeCast(null)
      testOutput<typeof validator._TYPE, string | null | undefined>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        required: true,
      })
    })
    test("multiselect", async () => {
      const value = Value.dynamicMultiselect(() => ({
        name: "Testing",
        values: {
          a: "A",
          b: "B",
        },
        default: [],
        description: null,
        warning: null,
        minLength: null,
        maxLength: null,
      }))
      const validator = value.validator
      validator.unsafeCast([])
      validator.unsafeCast(["a", "b"])
      validator.unsafeCast(["c"])

      expect(() => validator.unsafeCast([4])).toThrowError()
      expect(() => validator.unsafeCast(null)).toThrowError()
      testOutput<typeof validator._TYPE, Array<string>>()(null)
      expect(await value.build(fakeOptions)).toMatchObject({
        name: "Testing",
        default: [],
      })
    })
  })
  describe("filtering", () => {
    test("union", async () => {
      const value = Value.filteredUnion(
        () => ["a", "c"],
        {
          name: "Testing",
          required: { default: null },
          description: null,
          warning: null,
        },
        Variants.of({
          a: {
            name: "a",
            spec: Config.of({
              b: Value.toggle({
                name: "b",
                description: null,
                warning: null,
                default: false,
              }),
            }),
          },
          b: {
            name: "b",
            spec: Config.of({
              b: Value.toggle({
                name: "b",
                description: null,
                warning: null,
                default: false,
              }),
            }),
          },
        }),
      )
      const validator = value.validator
      validator.unsafeCast({ unionSelectKey: "a", unionValueKey: { b: false } })
      type Test = typeof validator._TYPE
      testOutput<
        Test,
        | { unionSelectKey: "a"; unionValueKey: { b: boolean } }
        | { unionSelectKey: "b"; unionValueKey: { b: boolean } }
      >()(null)

      const built = await value.build({} as any)
      expect(built).toMatchObject({
        name: "Testing",
        variants: {
          b: {},
        },
      })
      expect(built).toMatchObject({
        name: "Testing",
        variants: {
          a: {},
          b: {},
        },
      })
      expect(built).toMatchObject({
        name: "Testing",
        variants: {
          a: {},
          b: {},
        },
        disabled: ["a", "c"],
      })
    })
  })
  test("dynamic union", async () => {
    const value = Value.dynamicUnion(
      () => ({
        disabled: ["a", "c"],
        name: "Testing",
        required: { default: null },
        description: null,
        warning: null,
      }),
      Variants.of({
        a: {
          name: "a",
          spec: Config.of({
            b: Value.toggle({
              name: "b",
              description: null,
              warning: null,
              default: false,
            }),
          }),
        },
        b: {
          name: "b",
          spec: Config.of({
            b: Value.toggle({
              name: "b",
              description: null,
              warning: null,
              default: false,
            }),
          }),
        },
      }),
    )
    const validator = value.validator
    validator.unsafeCast({ unionSelectKey: "a", unionValueKey: { b: false } })
    type Test = typeof validator._TYPE
    testOutput<
      Test,
      | { unionSelectKey: "a"; unionValueKey: { b: boolean } }
      | { unionSelectKey: "b"; unionValueKey: { b: boolean } }
      | null
      | undefined
    >()(null)

    const built = await value.build({} as any)
    expect(built).toMatchObject({
      name: "Testing",
      variants: {
        b: {},
      },
    })
    expect(built).toMatchObject({
      name: "Testing",
      variants: {
        a: {},
        b: {},
      },
    })
    expect(built).toMatchObject({
      name: "Testing",
      variants: {
        a: {},
        b: {},
      },
      disabled: ["a", "c"],
    })
  })
})

describe("Builder List", () => {
  test("obj", async () => {
    const value = Value.list(
      List.obj(
        {
          name: "test",
        },
        {
          spec: Config.of({
            test: Value.toggle({
              name: "test",
              description: null,
              warning: null,
              default: false,
            }),
          }),
        },
      ),
    )
    const validator = value.validator
    validator.unsafeCast([{ test: true }])
    testOutput<typeof validator._TYPE, { test: boolean }[]>()(null)
  })
  test("text", async () => {
    const value = Value.list(
      List.text(
        {
          name: "test",
        },
        {
          patterns: [],
        },
      ),
    )
    const validator = value.validator
    validator.unsafeCast(["test", "text"])
    testOutput<typeof validator._TYPE, string[]>()(null)
  })
  describe("dynamic", () => {
    test("text", async () => {
      const value = Value.list(
        List.dynamicText(() => ({
          name: "test",
          spec: { patterns: [] },
        })),
      )
      const validator = value.validator
      validator.unsafeCast(["test", "text"])
      expect(() => validator.unsafeCast([3, 4])).toThrowError()
      expect(() => validator.unsafeCast(null)).toThrowError()
      testOutput<typeof validator._TYPE, string[]>()(null)
      expect(await value.build({} as any)).toMatchObject({
        name: "test",
        spec: { patterns: [] },
      })
    })
  })
  test("number", async () => {
    const value = Value.list(
      List.dynamicNumber(() => ({
        name: "test",
        spec: { integer: true },
      })),
    )
    const validator = value.validator
    expect(() => validator.unsafeCast(["test", "text"])).toThrowError()
    validator.unsafeCast([4, 2])
    expect(() => validator.unsafeCast(null)).toThrowError()
    validator.unsafeCast([])
    testOutput<typeof validator._TYPE, number[]>()(null)
    expect(await value.build({} as any)).toMatchObject({
      name: "test",
      spec: { integer: true },
    })
  })
})

describe("Nested nullable values", () => {
  test("Testing text", async () => {
    const value = Config.of({
      a: Value.text({
        name: "Temp Name",
        description:
          "If no name is provided, the name from config will be used",
        required: false,
      }),
    })
    const validator = value.validator
    validator.unsafeCast({ a: null })
    validator.unsafeCast({ a: "test" })
    expect(() => validator.unsafeCast({ a: 4 })).toThrowError()
    testOutput<typeof validator._TYPE, { a: string | null | undefined }>()(null)
  })
  test("Testing number", async () => {
    const value = Config.of({
      a: Value.number({
        name: "Temp Name",
        description:
          "If no name is provided, the name from config will be used",
        required: false,
        warning: null,
        placeholder: null,
        integer: false,
        min: null,
        max: null,
        step: null,
        units: null,
      }),
    })
    const validator = value.validator
    validator.unsafeCast({ a: null })
    validator.unsafeCast({ a: 5 })
    expect(() => validator.unsafeCast({ a: "4" })).toThrowError()
    testOutput<typeof validator._TYPE, { a: number | null | undefined }>()(null)
  })
  test("Testing color", async () => {
    const value = Config.of({
      a: Value.color({
        name: "Temp Name",
        description:
          "If no name is provided, the name from config will be used",
        required: false,
        warning: null,
      }),
    })
    const validator = value.validator
    validator.unsafeCast({ a: null })
    validator.unsafeCast({ a: "5" })
    expect(() => validator.unsafeCast({ a: 4 })).toThrowError()
    testOutput<typeof validator._TYPE, { a: string | null | undefined }>()(null)
  })
  test("Testing select", async () => {
    const value = Config.of({
      a: Value.select({
        name: "Temp Name",
        description:
          "If no name is provided, the name from config will be used",
        required: false,
        warning: null,
        values: {
          a: "A",
        },
      }),
    })
    const higher = await Value.select({
      name: "Temp Name",
      description: "If no name is provided, the name from config will be used",
      required: false,
      warning: null,
      values: {
        a: "A",
      },
    }).build({} as any)

    const validator = value.validator
    validator.unsafeCast({ a: null })
    validator.unsafeCast({ a: "a" })
    expect(() => validator.unsafeCast({ a: "4" })).toThrowError()
    testOutput<typeof validator._TYPE, { a: "a" | null | undefined }>()(null)
  })
  test("Testing multiselect", async () => {
    const value = Config.of({
      a: Value.multiselect({
        name: "Temp Name",
        description:
          "If no name is provided, the name from config will be used",

        warning: null,
        default: [],
        values: {
          a: "A",
        },
        minLength: null,
        maxLength: null,
      }),
    })
    const validator = value.validator
    validator.unsafeCast({ a: [] })
    validator.unsafeCast({ a: ["a"] })
    expect(() => validator.unsafeCast({ a: ["4"] })).toThrowError()
    expect(() => validator.unsafeCast({ a: "4" })).toThrowError()
    testOutput<typeof validator._TYPE, { a: "a"[] }>()(null)
  })
})
