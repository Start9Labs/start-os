import { types as T } from "@start9labs/start-sdk"
import { Effects } from "../Models/Effects"
import { CallbackHolder } from "../Models/CallbackHolder"
import { Optional } from "ts-matches/lib/parsers/interfaces"

export type Procedure =
  | "/packageInit"
  | "/packageUninit"
  | "/backup/create"
  | "/backup/restore"
  | `/actions/${string}/getInput`
  | `/actions/${string}/run`

export type ExecuteResult =
  | { ok: unknown }
  | { err: { code: number; message: string } }
export type System = {
  containerInit(effects: T.Effects): Promise<void>

  start(effects: T.Effects): Promise<void>
  stop(): Promise<void>

  packageInit(effects: Effects, timeoutMs: number | null): Promise<void>
  packageUninit(
    effects: Effects,
    nextVersion: Optional<string>,
    timeoutMs: number | null,
  ): Promise<void>

  createBackup(effects: T.Effects, timeoutMs: number | null): Promise<void>
  restoreBackup(effects: T.Effects, timeoutMs: number | null): Promise<void>
  runAction(
    effects: Effects,
    actionId: string,
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
