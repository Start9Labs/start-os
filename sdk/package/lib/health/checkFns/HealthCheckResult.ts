import { T } from '../../../../base/lib'

/**
 * The result of a single health check invocation.
 *
 * Contains a `result` field ("success", "failure", or "starting") and an optional `message`.
 * This is the unnamed variant -- the health check name is added by the framework.
 */
export type HealthCheckResult = Omit<T.NamedHealthCheckResult, 'name'>
