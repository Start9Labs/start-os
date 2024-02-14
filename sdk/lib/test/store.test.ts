import { Effects } from "../types"
import { createMainUtils } from "../util"
import { createUtils } from "../util/utils"

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
describe("Store", () => {
  test("types", async () => {
    ;async () => {
      createUtils<Manifest, Store>(todo<Effects>()).store.setOwn("/config", {
        someValue: "a",
      })
      createUtils<Manifest, Store>(todo<Effects>()).store.setOwn(
        "/config/someValue",
        "b",
      )
      createUtils<Manifest, Store>(todo<Effects>()).store.setOwn("", {
        config: { someValue: "b" },
      })
      createUtils<Manifest, Store>(todo<Effects>()).store.setOwn(
        "/config/someValue",

        // @ts-expect-error Type is wrong for the setting value
        5,
      )
      createUtils(todo<Effects>()).store.setOwn(
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
      ;(await createMainUtils<Manifest, Store>(todo<Effects>())
        .store.getOwn("/config/someValue")
        .const()) satisfies string
      ;(await createMainUtils<Manifest, Store>(todo<Effects>())
        .store.getOwn("/config")
        .const()) satisfies Store["config"]
      await createMainUtils(todo<Effects>())
        // @ts-expect-error Path is wrong
        .store.getOwn("/config/somdsfeValue")
        .const()
      ///  ----------------- ERRORS -----------------

      createUtils<Manifest, Store>(todo<Effects>()).store.setOwn("", {
        // @ts-expect-error Type is wrong for the setting value
        config: { someValue: "notInAOrB" },
      })
      createUtils<Manifest, Store>(todo<Effects>()).store.setOwn(
        "/config/someValue",
        // @ts-expect-error Type is wrong for the setting value
        "notInAOrB",
      )
      ;(await createUtils<Manifest, Store>(todo<Effects>())
        .store.getOwn("/config/someValue")
        // @ts-expect-error Const should normally not be callable
        .const()) satisfies string
      ;(await createUtils<Manifest, Store>(todo<Effects>())
        .store.getOwn("/config")
        // @ts-expect-error Const should normally not be callable
        .const()) satisfies Store["config"]
      await createUtils<Manifest, Store>(todo<Effects>())
        // @ts-expect-error Path is wrong
        .store.getOwn("/config/somdsfeValue")
        // @ts-expect-error Const should normally not be callable
        .const()

      ///
      ;(await createUtils<Manifest, Store>(todo<Effects>())
        .store.getOwn("/config/someValue")
        // @ts-expect-error satisfies type is wrong
        .const()) satisfies number
      ;(await createMainUtils(todo<Effects>())
        // @ts-expect-error Path is wrong
        .store.getOwn("/config/")
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
