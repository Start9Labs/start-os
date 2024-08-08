import { T } from "../.."

export type HealthCheckResult = Omit<T.NamedHealthCheckResult, "name">
