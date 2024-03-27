import { MainEffects, StartSdk } from "../StartSdk"
import { extractJsonPath } from "../store/PathBuilder"
import { Effects } from "../types"

type Store = {
  config: {
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
      sdk.store.setOwn(todo<Effects>(), storePath.config, {
        someValue: "a",
      })
      sdk.store.setOwn(todo<Effects>(), storePath.config.someValue, "b")
      sdk.store.setOwn(todo<Effects>(), storePath, {
        config: { someValue: "b" },
      })
      sdk.store.setOwn(
        todo<Effects>(),
        storePath.config.someValue,

        // @ts-expect-error Type is wrong for the setting value
        5,
      )
      sdk.store.setOwn(
        todo<Effects>(),
        // @ts-expect-error Path is wrong
        "/config/someVae3lue",
        "someValue",
      )

      todo<Effects>().store.set<Store>({
        path: extractJsonPath(storePath.config.someValue),
        value: "b",
      })
      todo<Effects>().store.set<Store, "/config/some2Value">({
        path: extractJsonPath(storePath.config.someValue),
        //@ts-expect-error Path is wrong
        value: "someValueIn",
      })
      ;(await sdk.store
        .getOwn(todo<MainEffects>(), storePath.config.someValue)
        .const()) satisfies string
      ;(await sdk.store
        .getOwn(todo<MainEffects>(), storePath.config)
        .const()) satisfies Store["config"]
      await sdk.store // @ts-expect-error Path is wrong
        .getOwn(todo<MainEffects>(), "/config/somdsfeValue")
        .const()
      ///  ----------------- ERRORS -----------------

      sdk.store.setOwn(todo<MainEffects>(), storePath, {
        // @ts-expect-error Type is wrong for the setting value
        config: { someValue: "notInAOrB" },
      })
      sdk.store.setOwn(
        todo<MainEffects>(),
        sdk.StorePath.config.someValue,
        // @ts-expect-error Type is wrong for the setting value
        "notInAOrB",
      )
      ;(await sdk.store
        .getOwn(todo<Effects>(), storePath.config.someValue)
        // @ts-expect-error Const should normally not be callable
        .const()) satisfies string
      ;(await sdk.store
        .getOwn(todo<Effects>(), storePath.config)
        // @ts-expect-error Const should normally not be callable
        .const()) satisfies Store["config"]
      await sdk.store // @ts-expect-error Path is wrong
        .getOwn("/config/somdsfeValue")
        // @ts-expect-error Const should normally not be callable
        .const()

      ///
      ;(await sdk.store
        .getOwn(todo<MainEffects>(), storePath.config.someValue)
        // @ts-expect-error satisfies type is wrong
        .const()) satisfies number
      await sdk.store // @ts-expect-error Path is wrong
        .getOwn(todo<MainEffects>(), extractJsonPath(storePath.config))
        .const()
      ;(await todo<Effects>().store.get({
        path: extractJsonPath(storePath.config.someValue),
        callback: noop,
      })) satisfies string
      await todo<Effects>().store.get<Store, "/config/someValue">({
        // @ts-expect-error Path is wrong as in it doesn't match above
        path: "/config/someV2alue",
        callback: noop,
      })
      await todo<Effects>().store.get<Store, "/config/someV2alue">({
        // @ts-expect-error Path is wrong as in it doesn't exists in wrapper type
        path: "/config/someV2alue",
        callback: noop,
      })
    }
  })
})
