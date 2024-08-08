import { matchOldConfigSpec, transformConfigSpec } from "./transformConfigSpec"
import fixtureEmbasyPagesConfig from "./__fixtures__/embasyPagesConfig"
import searNXG from "./__fixtures__/searNXG"
import bitcoind from "./__fixtures__/bitcoind"
import nostr from "./__fixtures__/nostr"

describe("transformConfigSpec", () => {
  test("matchOldConfigSpec(embassyPages.homepage.variants[web-page])", () => {
    matchOldConfigSpec.unsafeCast(
      fixtureEmbasyPagesConfig.homepage.variants["web-page"],
    )
  })
  test("matchOldConfigSpec(embassyPages)", () => {
    matchOldConfigSpec.unsafeCast(fixtureEmbasyPagesConfig)
  })
  test("transformConfigSpec(embassyPages)", () => {
    const spec = matchOldConfigSpec.unsafeCast(fixtureEmbasyPagesConfig)
    expect(transformConfigSpec(spec)).toMatchSnapshot()
  })

  test("transformConfigSpec(searNXG)", () => {
    const spec = matchOldConfigSpec.unsafeCast(searNXG)
    expect(transformConfigSpec(spec)).toMatchSnapshot()
  })
  test("transformConfigSpec(bitcoind)", () => {
    const spec = matchOldConfigSpec.unsafeCast(bitcoind)
    expect(transformConfigSpec(spec)).toMatchSnapshot()
  })
  test("transformConfigSpec(nostr)", () => {
    const spec = matchOldConfigSpec.unsafeCast(nostr)
    expect(transformConfigSpec(spec)).toMatchSnapshot()
  })
})
