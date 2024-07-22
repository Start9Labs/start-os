import { types as T } from "@start9labs/start-sdk"
import { JsonPath } from "../Models/JsonPath"
import { RpcResult } from "../Adapters/RpcListener"
import { hostSystemStartOs } from "../Adapters/HostSystemStartOs"
import { Optional } from "ts-matches/lib/parsers/interfaces"
type Effects = T.Effects
export type ExecuteResult =
  | { ok: unknown }
  | { err: { code: number; message: string } }
export type System = {
  // execute(
  //   effectCreator: ReturnType<typeof hostSystemStartOs>,
  //   options: {
  //     id: string
  //     procedure: JsonPath
  //     input: unknown
  //     timeout?: number
  //   },
  // ): Promise<RpcResult>
  mainStart(effects: Effects, timeoutMs: number | null): Promise<void>
  mainStop(effects: Effects, timeoutMs: number | null): Promise<void>

  init(
    effects: Effects,
    previousVersion: Optional<string>,
    timeoutMs: number | null,
  ): Promise<void>
  uninit(
    effects: Effects,
    nextVersion: Optional<string>,
    timeoutMs: number | null,
  ): Promise<void>

  createBackup(effects: T.Effects, timeoutMs: number | null): Promise<void>
  restoreBackup(effects: T.Effects, timeoutMs: number | null): Promise<void>
  getConfig(effects: T.Effects, timeoutMs: number | null): Promise<T.ConfigRes>
  setConfig(
    effects: Effects,
    newConfigWithoutPointers: unknown,
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
  ): Promise<T.ActionResult>

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
  dependenciesMetadata(effects: T.Effects): Promise<T.ActionMetadata[]>

  exit(effects: T.Effects): Promise<void>
}
