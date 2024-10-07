import { T } from "../../../../base/lib"

export type HealthCheckResult = Omit<T.NamedHealthCheckResult, "name">
