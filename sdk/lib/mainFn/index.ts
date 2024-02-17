import { Effects, ExpectedExports } from "../types"
import { createMainUtils } from "../util"
import { Utils, createUtils } from "../util/utils"
import { Daemons } from "./Daemons"
import "../interfaces/NetworkInterfaceBuilder"
import "../interfaces/Origin"

import "./Daemons"
import { SDKManifest } from "../manifest/ManifestTypes"

/**
 * Used to ensure that the main function is running with the valid proofs.
 * We first do the folowing order of things
 * 1. We get the interfaces
 * 2. We setup all the commands to setup the system
 * 3. We create the health checks
 * 4. We setup the daemons init system
 * @param fn
 * @returns
 */
export const setupMain = <Manifest extends SDKManifest, Store>(
  fn: (o: {
    effects: Effects
    started(onTerm: () => PromiseLike<void>): PromiseLike<void>
    utils: Utils<Manifest, Store, {}>
  }) => Promise<Daemons<Manifest, any>>,
): ExpectedExports.main => {
  return async (options) => {
    const result = await fn({
      ...options,
      utils: createMainUtils<Manifest, Store>(options.effects),
    })
    return result
  }
}
