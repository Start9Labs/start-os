import { HealthStatus } from "../types"

export type TriggerInput = {
  lastResult?: HealthStatus
  hadSuccess?: boolean
}
