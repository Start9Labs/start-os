import { types as T } from "@start9labs/start-sdk"
import { RpcResult } from "../Adapters/RpcListener"
import { Effects } from "../Models/Effects"
import { CallbackHolder } from "../Models/CallbackHolder"
import { MainEffects } from "@start9labs/start-sdk/cjs/lib/StartSdk"

export type Procedure =
  | "/init"
  | "/uninit"
  | "/config/set"
  | "/config/get"
  | "/backup/create"
  | "/backup/restore"
  | "/actions/metadata"
  | "/properties"
  | `/actions/${string}/get`
  | `/actions/${string}/run`
  | `/dependencies/${string}/query`
  | `/dependencies/${string}/update`

export type ExecuteResult =
  | { ok: unknown }
  | { err: { code: number; message: string } }
export type System = {
  init(): Promise<void>

  start(effects: MainEffects): Promise<void>
  callCallback(callback: number, args: any[]): void
  stop(): Promise<void>

  execute(
    effects: Effects,
    options: {
      procedure: Procedure
      input: unknown
      timeout?: number
    },
  ): Promise<RpcResult>
  sandbox(
    effects: Effects,
    options: {
      procedure: Procedure
      input: unknown
      timeout?: number
    },
  ): Promise<RpcResult>

  exit(): Promise<void>
}

export type RunningMain = {
  callbacks: CallbackHolder
  stop(): Promise<void>
}
