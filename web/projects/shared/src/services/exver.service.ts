import { Injectable } from '@angular/core'
import { VersionRange, ExtendedVersion } from '@start9labs/start-sdk'

@Injectable({
  providedIn: 'root',
})
export class Exver {
  constructor() {}

  compare(lhs: string, rhs: string): number | null {
    if (!lhs || !rhs) return null
    return ExtendedVersion.parse(lhs).compareForSort(ExtendedVersion.parse(rhs))
  }

  satisfies(version: string, range: string): boolean {
    return VersionRange.parse(range).satisfiedBy(ExtendedVersion.parse(version))
  }
}
