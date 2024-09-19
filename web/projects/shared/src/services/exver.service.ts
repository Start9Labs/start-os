import { Injectable } from '@angular/core'
import { ExtendedVersion } from '@start9labs/start-sdk'

@Injectable({
  providedIn: 'root',
})
export class Exver {
  constructor() {}

  compareExver(lhs: string, rhs: string): number | null {
    if (!lhs || !rhs) return null
    try {
      return ExtendedVersion.parse(lhs).compareForSort(
        ExtendedVersion.parse(rhs),
      )
    } catch (e) {
      return null
    }
  }

  greaterThanOrEqual(lhs: string, rhs: string): boolean | null {
    if (!lhs || !rhs) return null
    try {
      return ExtendedVersion.parse(lhs).greaterThanOrEqual(
        ExtendedVersion.parse(rhs),
      )
    } catch (e) {
      return null
    }
  }

  satisfies(version: string, range: string): boolean {
    return ExtendedVersion.parse(version).satisfies(range)
  }

  getFlavor(version: string): string | null {
    return ExtendedVersion.parse(version).flavor
  }
}
