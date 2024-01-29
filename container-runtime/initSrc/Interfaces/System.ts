import { Effects } from "@start9labs/start-sdk/lib/types"
import { JsonPath } from "../Models/JsonPath"

export type ExecuteResult =
  | { ok: unknown }
  | { err: { code: number; message: string } }
export interface System {
  // init(effects: Effects): Promise<void>
  // exit(effects: Effects): Promise<void>
  // start(effects: Effects): Promise<void>
  // stop(effects: Effects, options: { timeout: number, signal?: number }): Promise<void>

  execute(
    effects: Effects,
    options: {
      procedure: JsonPath
      input: unknown
      timeout?: number
    },
  ): Promise<ExecuteResult>
  // sandbox(
  //   effects: Effects,
  //   options: {
  //     procedure: JsonPath
  //     input: unknown
  //     timeout?: number
  //   },
  // ): Promise<unknown>

  exit(effects: Effects): Promise<void>
}
