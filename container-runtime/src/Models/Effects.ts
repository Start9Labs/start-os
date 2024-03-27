import { types as T } from "@start9labs/start-sdk"

export type Effects = T.Effects & {
  setMainStatus(o: { status: "running" | "stopped" }): Promise<void>
}
