import { Injectable } from '@angular/core'
import { VersionRange, ExtendedVersion, Version } from '@start9labs/start-sdk'

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

  compareOsVersion(current: string, other: string) {
    return Version.parse(current).compare(Version.parse(other))
  }

  satisfies(version: string, range: string): boolean {
    return VersionRange.parse(range).satisfiedBy(ExtendedVersion.parse(version))
  }

  getFlavor(version: string): string | null {
    return ExtendedVersion.parse(version).flavor
  }
}
