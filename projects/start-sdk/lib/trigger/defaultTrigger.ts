import { statusTrigger } from './statusTrigger'

/**
 * The trigger used when no explicit `trigger` is set on a health check.
 *
 * Polls every 1 s while `starting`, `waiting`, or `failure`, then every
 * 30 s for `success`, `loading`, or `disabled`.
 */
export const defaultTrigger = statusTrigger(30_000, {
  starting: 1_000,
  waiting: 1_000,
  failure: 1_000,
})
