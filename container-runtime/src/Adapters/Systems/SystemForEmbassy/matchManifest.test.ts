import { matchManifest } from "./matchManifest"
import giteaManifest from "./__fixtures__/giteaManifest"
import synapseManifest from "./__fixtures__/synapseManifest"

describe("matchManifest", () => {
  test("gittea", () => {
    matchManifest.unsafeCast(giteaManifest)
  })
  test("synapse", () => {
    matchManifest.unsafeCast(synapseManifest)
  })
})
