import { MainEffects, StartSdk } from "../StartSdk"
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

describe("Store", () => {
  test("types", async () => {
    ;async () => {
      sdk.store.setOwn(todo<Effects>(), "/config", {
        someValue: "a",
      })
      sdk.store.setOwn(todo<Effects>(), "/config/someValue", "b")
      sdk.store.setOwn(todo<Effects>(), "", {
        config: { someValue: "b" },
      })
      sdk.store.setOwn(
        todo<Effects>(),
        "/config/someValue",

        // @ts-expect-error Type is wrong for the setting value
        5,
      )
      sdk.store.setOwn(
        todo<Effects>(),
        // @ts-expect-error Path is wrong
        "/config/someVae3lue",
        "someValue",
      )

      todo<Effects>().store.set<Store, "/config/someValue">({
        path: "/config/someValue",
        value: "b",
      })
      todo<Effects>().store.set<Store, "/config/some2Value">({
        //@ts-expect-error Path is wrong
        path: "/config/someValue",
        //@ts-expect-error Path is wrong
        value: "someValueIn",
      })
      todo<Effects>().store.set<Store, "/config/someValue">({
        //@ts-expect-error Path is wrong
        path: "/config/some2Value",
        value: "a",
      })
      ;(await sdk.store
        .getOwn(todo<MainEffects>(), "/config/someValue")
        .const()) satisfies string
      ;(await sdk.store
        .getOwn(todo<MainEffects>(), "/config")
        .const()) satisfies Store["config"]
      await sdk.store // @ts-expect-error Path is wrong
        .getOwn(todo<MainEffects>(), "/config/somdsfeValue")
        .const()
      ///  ----------------- ERRORS -----------------

      sdk.store.setOwn(todo<MainEffects>(), "", {
        // @ts-expect-error Type is wrong for the setting value
        config: { someValue: "notInAOrB" },
      })
      sdk.store.setOwn(
        todo<MainEffects>(),
        "/config/someValue",
        // @ts-expect-error Type is wrong for the setting value
        "notInAOrB",
      )
      ;(await sdk.store
        .getOwn(todo<Effects>(), "/config/someValue")
        // @ts-expect-error Const should normally not be callable
        .const()) satisfies string
      ;(await sdk.store
        .getOwn(todo<Effects>(), "/config")
        // @ts-expect-error Const should normally not be callable
        .const()) satisfies Store["config"]
      await sdk.store // @ts-expect-error Path is wrong
        .getOwn("/config/somdsfeValue")
        // @ts-expect-error Const should normally not be callable
        .const()

      ///
      ;(await sdk.store
        .getOwn(todo<MainEffects>(), "/config/someValue")
        // @ts-expect-error satisfies type is wrong
        .const()) satisfies number
      ;(await sdk.store // @ts-expect-error Path is wrong
        .getOwn(todo<MainEffects>(), "/config/")
        .const()) satisfies Store["config"]
      ;(await todo<Effects>().store.get<Store, "/config/someValue">({
        path: "/config/someValue",
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
