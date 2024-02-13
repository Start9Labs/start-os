import { types as T } from "@start9labs/start-sdk"

import { CallbackHolder } from "../Models/CallbackHolder"
import { Effects } from "../Models/Effects"

export type HostSystem = Effects
export type GetHostSystem = (callbackHolder: CallbackHolder) => HostSystem
