import { HealthStatus } from "../../types"

export type CheckResult = {
  status: HealthStatus
  message?: string
}
