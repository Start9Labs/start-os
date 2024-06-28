import { types as T } from "@start9labs/start-sdk"
import { JsonPath } from "../Models/JsonPath"
import { RpcResult } from "../Adapters/RpcListener"
import { hostSystemStartOs } from "../Adapters/HostSystemStartOs"
export type ExecuteResult =
  | { ok: unknown }
  | { err: { code: number; message: string } }
export type System = {
  // init(effects: Effects): Promise<void>
  // exit(effects: Effects): Promise<void>
  // start(effects: Effects): Promise<void>
  // stop(effects: Effects, options: { timeout: number, signal?: number }): Promise<void>

  execute(
    effectCreator: ReturnType<typeof hostSystemStartOs>,
    options: {
      id: string
      procedure: JsonPath
      input: unknown
      timeout?: number
    },
  ): Promise<RpcResult>
  // sandbox(
  //   effects: Effects,
  //   options: {
  //     procedure: JsonPath
  //     input: unknown
  //     timeout?: number
  //   },
  // ): Promise<unknown>

  exit(effects: T.Effects): Promise<void>
}
