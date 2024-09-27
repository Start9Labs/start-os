import * as T from "../../../base/lib/types"
import { Daemons } from "./Daemons"
import "../../../base/lib/interfaces/ServiceInterfaceBuilder"
import "../../../base/lib/interfaces/Origin"

export const DEFAULT_SIGTERM_TIMEOUT = 30_000
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
export const setupMain = <Manifest extends T.Manifest, Store>(
  fn: (o: {
    effects: T.Effects
    started(onTerm: () => PromiseLike<void>): PromiseLike<null>
  }) => Promise<Daemons<Manifest, any>>,
): T.ExpectedExports.main => {
  return async (options) => {
    const result = await fn(options)
    return result
  }
}
