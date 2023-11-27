import * as T from "@start9labs/start-sdk/lib/types"

import { CallbackHolder } from "../Models/CallbackHolder"

export type HostSystem = T.Effects
export type GetHostSystem = (
  method: string,
  callbackHolder: CallbackHolder,
) => HostSystem
