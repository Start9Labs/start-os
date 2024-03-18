import { Effects, ExposeServicePaths, ExposeUiPaths } from "../types"
import { Utils } from "../util/utils"

export type SetupExports<Store> = (opts: {
  effects: Effects
  utils: Utils<any, Store>
}) =>
  | {
      ui: { [k: string]: ExposeUiPaths<Store> }
      services: ExposeServicePaths<Store>
    }
  | Promise<{
      ui: { [k: string]: ExposeUiPaths<Store> }
      services: ExposeServicePaths<Store>
    }>

export const setupExports = <Store>(fn: (opts: SetupExports<Store>) => void) =>
  fn
