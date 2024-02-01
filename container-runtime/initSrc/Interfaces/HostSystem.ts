import * as T from "@start9labs/start-sdk/lib/types"

import { CallbackHolder } from "../Models/CallbackHolder"
import { Effects } from "../Models/ Effects"

export type HostSystem = Effects
export type GetHostSystem = (callbackHolder: CallbackHolder) => HostSystem
