// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { HealthCheckId } from "./HealthCheckId"
import type { NamedHealthCheckResult } from "./NamedHealthCheckResult"
import type { StartStop } from "./StartStop"

export type MainStatus =
  | { main: "stopped" }
  | { main: "restarting" }
  | { main: "restoring" }
  | { main: "stopping" }
  | {
      main: "starting"
      health: { [key: HealthCheckId]: NamedHealthCheckResult }
    }
  | {
      main: "running"
      started: string
      health: { [key: HealthCheckId]: NamedHealthCheckResult }
    }
  | { main: "backingUp"; onComplete: StartStop }
