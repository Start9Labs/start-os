import { ServiceInterfaceBuilder } from "../interfaces/ServiceInterfaceBuilder"
import { Effects } from "../Effects"
import { sdk } from "./output.sdk"

describe("host", () => {
  test("Testing that the types work", () => {
    async function test(effects: Effects) {
      const foo = sdk.host.multi(effects, "foo")
      const fooOrigin = await foo.bindPort(80, {
        protocol: "http" as const,
        preferredExternalPort: 80,
      })
      const fooInterface = new ServiceInterfaceBuilder({
        effects,
        name: "Foo",
        id: "foo",
        description: "A Foo",
        hasPrimary: false,
        type: "ui",
        username: "bar",
        path: "/baz",
        search: { qux: "yes" },
        schemeOverride: null,
        masked: false,
      })

      await fooOrigin.export([fooInterface])
    }
  })
})
