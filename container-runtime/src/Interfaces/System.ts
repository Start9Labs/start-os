import { types as T } from "@start9labs/start-sdk"
import { RpcResult } from "../Adapters/RpcListener"
import { Effects } from "../Models/Effects"
import { CallbackHolder } from "../Models/CallbackHolder"
import { MainEffects } from "@start9labs/start-sdk/cjs/lib/StartSdk"
import { Optional } from "ts-matches/lib/parsers/interfaces"

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
  containerInit(): Promise<void>

  start(effects: MainEffects): Promise<void>
  callCallback(callback: number, args: any[]): void
  stop(): Promise<void>

  packageInit(
    effects: Effects,
    previousVersion: Optional<string>,
    timeoutMs: number | null,
  ): Promise<void>
  packageUninit(
    effects: Effects,
    nextVersion: Optional<string>,
    timeoutMs: number | null,
  ): Promise<void>

  createBackup(effects: T.Effects, timeoutMs: number | null): Promise<void>
  restoreBackup(effects: T.Effects, timeoutMs: number | null): Promise<void>
  getConfig(effects: T.Effects, timeoutMs: number | null): Promise<T.ConfigRes>
  setConfig(
    effects: Effects,
    input: { effects: Effects; input: Record<string, unknown> },
    timeoutMs: number | null,
  ): Promise<void>
  migration(
    effects: Effects,
    fromVersion: string,
    timeoutMs: number | null,
  ): Promise<T.MigrationRes>
  properties(
    effects: Effects,
    timeoutMs: number | null,
  ): Promise<T.PropertiesReturn>
  action(
    effects: Effects,
    actionId: string,
    formData: unknown,
    timeoutMs: number | null,
  ): Promise<T.configTypes.InputSpec>

  dependenciesCheck(
    effects: Effects,
    id: string,
    oldConfig: unknown,
    timeoutMs: number | null,
  ): Promise<any>
  dependenciesAutoconfig(
    effects: Effects,
    id: string,
    oldConfig: unknown,
    timeoutMs: number | null,
  ): Promise<void>
  actionsMetadata(effects: T.Effects): Promise<T.ActionMetadata[]>

  exit(): Promise<void>
}

export type RunningMain = {
  callbacks: CallbackHolder
  stop(): Promise<void>
}
