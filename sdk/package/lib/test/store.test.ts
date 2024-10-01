import { Effects } from "../../../base/lib/types"
import { extractJsonPath } from "../../../base/lib/util/PathBuilder"
import { StartSdk } from "../StartSdk"

type Store = {
  inputSpec: {
    someValue: "a" | "b"
  }
}
type Manifest = any
const todo = <A>(): A => {
  throw new Error("not implemented")
}
const noop = () => {}

const sdk = StartSdk.of()
  .withManifest({} as Manifest)
  .withStore<Store>()
  .build(true)

const storePath = sdk.StorePath

describe("Store", () => {
  test("types", async () => {
    ;async () => {
      sdk.store.setOwn(todo<Effects>(), storePath.inputSpec, {
        someValue: "a",
      })
      sdk.store.setOwn(todo<Effects>(), storePath.inputSpec.someValue, "b")
      sdk.store.setOwn(todo<Effects>(), storePath, {
        inputSpec: { someValue: "b" },
      })
      sdk.store.setOwn(
        todo<Effects>(),
        storePath.inputSpec.someValue,

        // @ts-expect-error Type is wrong for the setting value
        5,
      )
      sdk.store.setOwn(
        todo<Effects>(),
        // @ts-expect-error Path is wrong
        "/inputSpec/someVae3lue",
        "someValue",
      )

      todo<Effects>().store.set<Store>({
        path: extractJsonPath(storePath.inputSpec.someValue),
        value: "b",
      })
      todo<Effects>().store.set<Store, "/inputSpec/some2Value">({
        path: extractJsonPath(storePath.inputSpec.someValue),
        //@ts-expect-error Path is wrong
        value: "someValueIn",
      })
      ;(await sdk.store
        .getOwn(todo<Effects>(), storePath.inputSpec.someValue)
        .const()) satisfies string
      ;(await sdk.store
        .getOwn(todo<Effects>(), storePath.inputSpec)
        .const()) satisfies Store["inputSpec"]
      await sdk.store // @ts-expect-error Path is wrong
        .getOwn(todo<Effects>(), "/inputSpec/somdsfeValue")
        .const()
      ///  ----------------- ERRORS -----------------

      sdk.store.setOwn(todo<Effects>(), storePath, {
        // @ts-expect-error Type is wrong for the setting value
        inputSpec: { someValue: "notInAOrB" },
      })
      sdk.store.setOwn(
        todo<Effects>(),
        sdk.StorePath.inputSpec.someValue,
        // @ts-expect-error Type is wrong for the setting value
        "notInAOrB",
      )
      ;(await sdk.store
        .getOwn(todo<Effects>(), storePath.inputSpec.someValue)
        .const()) satisfies string
      ;(await sdk.store
        .getOwn(todo<Effects>(), storePath.inputSpec)
        .const()) satisfies Store["inputSpec"]
      await sdk.store // @ts-expect-error Path is wrong
        .getOwn("/inputSpec/somdsfeValue")
        .const()

      ///
      ;(await sdk.store
        .getOwn(todo<Effects>(), storePath.inputSpec.someValue)
        // @ts-expect-error satisfies type is wrong
        .const()) satisfies number
      await sdk.store // @ts-expect-error Path is wrong
        .getOwn(todo<Effects>(), extractJsonPath(storePath.inputSpec))
        .const()
      ;(await todo<Effects>().store.get({
        path: extractJsonPath(storePath.inputSpec.someValue),
        callback: noop,
      })) satisfies string
      await todo<Effects>().store.get<Store, "/inputSpec/someValue">({
        // @ts-expect-error Path is wrong as in it doesn't match above
        path: "/inputSpec/someV2alue",
        callback: noop,
      })
      await todo<Effects>().store.get<Store, "/inputSpec/someV2alue">({
        // @ts-expect-error Path is wrong as in it doesn't exists in wrapper type
        path: "/inputSpec/someV2alue",
        callback: noop,
      })
    }
  })
})
