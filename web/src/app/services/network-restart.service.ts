import { Injectable } from '@angular/core'

@Injectable({ providedIn: 'root' })
export class NetworkRestartService {
  private suppressed = false

  suppress(): void {
    this.suppressed = true
  }

  recovered(): void {
    this.suppressed = false
  }

  get isSuppressed(): boolean {
    return this.suppressed
  }
}

export function isNetworkError(e: unknown): boolean {
  return (
    e != null && typeof e === 'object' && 'code' in e && (e as any).code === 0
  )
}
