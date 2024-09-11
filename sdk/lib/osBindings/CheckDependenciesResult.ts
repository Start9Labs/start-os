// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { ActionRequestEntry } from "./ActionRequestEntry"
import type { HealthCheckId } from "./HealthCheckId"
import type { NamedHealthCheckResult } from "./NamedHealthCheckResult"
import type { PackageId } from "./PackageId"

export type CheckDependenciesResult = {
  packageId: PackageId
  title: string | null
  installedVersion: string | null
  satisfies: string[]
  isRunning: boolean
  requestedActions: { [key: string]: ActionRequestEntry }
  healthChecks: { [key: HealthCheckId]: NamedHealthCheckResult }
}
