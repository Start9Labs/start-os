import { Effects } from "@start9labs/start-sdk/lib/types"
import { JsonPath } from "../Models/JsonPath"

export interface System {
  init(effects: Effects): Promise<void>
  exit(effects: Effects): Promise<void>
  start(effects: Effects): Promise<void>
  stop(effects: Effects, options: { timeout: number }): Promise<void>

  execute(
    effects: Effects,
    options: {
      procedure: JsonPath
      input: unknown
      timeout?: number
    },
  ): Promise<unknown>
  sandbox(
    effects: Effects,
    options: {
      procedure: JsonPath
      input: unknown
      timeout?: number
    },
  ): Promise<unknown>
}
