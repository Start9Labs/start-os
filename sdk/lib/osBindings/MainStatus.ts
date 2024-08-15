// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { HealthCheckId } from "./HealthCheckId"
import type { NamedHealthCheckResult } from "./NamedHealthCheckResult"

export type MainStatus =
  | { status: "stopped" }
  | { status: "restarting" }
  | { status: "restoring" }
  | { status: "stopping" }
  | { status: "starting" }
  | {
      status: "running"
      started: string
      health: { [key: HealthCheckId]: NamedHealthCheckResult }
    }
  | {
      status: "backingUp"
      started: string | null
      health: { [key: HealthCheckId]: NamedHealthCheckResult }
    }
