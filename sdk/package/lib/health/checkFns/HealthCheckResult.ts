import { T } from '../../../../base/lib'

/**
 * The value returned from a health check `fn`.
 *
 * Set `result` to one of:
 * - `'success'`  — healthy and fully operational
 * - `'loading'`  — operational but still catching up (e.g. syncing blocks)
 * - `'disabled'` — intentionally inactive (excluded by config)
 * - `'starting'` — not yet ready, still initializing
 * - `'waiting'`  — blocked on an external dependency
 * - `'failure'`  — unhealthy, something is wrong
 *
 * Include a `message` string for display in the StartOS UI. Required for
 * `loading` and `failure`; optional for other states.
 */
export type HealthCheckResult = Omit<T.NamedHealthCheckResult, 'name'>
