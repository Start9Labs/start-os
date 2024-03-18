import { Effects, ExposeServicePaths, ExposeUiPaths } from "../types"

export type SetupExports<Store> = (opts: { effects: Effects }) =>
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
