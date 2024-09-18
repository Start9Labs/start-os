import { types as T } from "@start9labs/start-sdk"
import { RpcResult } from "../Adapters/RpcListener"
import { Effects } from "../Models/Effects"
import { CallbackHolder } from "../Models/CallbackHolder"
import { Optional } from "ts-matches/lib/parsers/interfaces"

export type Procedure =
  | "/init"
  | "/uninit"
  | "/config/set"
  | "/config/get"
  | "/backup/create"
  | "/backup/restore"
  | "/properties"
  | `/actions/${string}/get`
  | `/actions/${string}/run`
  | `/dependencies/${string}/query`
  | `/dependencies/${string}/update`

export type ExecuteResult =
  | { ok: unknown }
  | { err: { code: number; message: string } }
export type System = {
  containerInit(effects: T.Effects): Promise<void>

  start(effects: T.MainEffects): Promise<void>
  callCallback(callback: number, args: any[]): void
  stop(): Promise<void>

  packageInit(effects: Effects, timeoutMs: number | null): Promise<void>
  packageUninit(
    effects: Effects,
    nextVersion: Optional<string>,
    timeoutMs: number | null,
  ): Promise<void>

  createBackup(effects: T.Effects, timeoutMs: number | null): Promise<void>
  restoreBackup(effects: T.Effects, timeoutMs: number | null): Promise<void>
  properties(
    effects: Effects,
    timeoutMs: number | null,
  ): Promise<T.PropertiesReturn>
  runAction(
    effects: Effects,
    actionId: string,
    prev: T.ActionInput | null,
    input: unknown,
    timeoutMs: number | null,
  ): Promise<T.ActionResult | null>
  getActionInput(
    effects: Effects,
    actionId: string,
    timeoutMs: number | null,
  ): Promise<T.ActionInput | null>

  exit(): Promise<void>
}

export type RunningMain = {
  callbacks: CallbackHolder
  stop(): Promise<void>
}
