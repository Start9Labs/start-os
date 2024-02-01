import * as T from "@start9labs/start-sdk/lib/types"

export type Effects = T.Effects & {
  setMainStatus(o: { status: "running" | "stopped" }): Promise<void>
}
