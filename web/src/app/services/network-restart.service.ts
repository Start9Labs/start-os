import { Injectable } from '@angular/core'

/**
 * Slightly more than the expected network restart duration on a Banana Pi F3.
 * Network services typically recover within ~10s; this adds margin.
 */
export const NETWORK_RESTART_TIMEOUT_MS = 15_000

@Injectable({ providedIn: 'root' })
export class NetworkRestartService {
  private suppressUntil = 0

  suppress(ms = NETWORK_RESTART_TIMEOUT_MS): void {
    this.suppressUntil = Date.now() + ms
  }

  get isSuppressed(): boolean {
    return Date.now() < this.suppressUntil
  }
}

export function isNetworkError(e: unknown): boolean {
  return (
    e != null && typeof e === 'object' && 'code' in e && (e as any).code === 0
  )
}
