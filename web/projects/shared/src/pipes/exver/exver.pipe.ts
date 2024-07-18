import { Pipe, PipeTransform } from '@angular/core'
import { Exver } from '../../services/exver.service'
import { ExtendedVersion, VersionRange } from '@start9labs/start-sdk'

@Pipe({
  name: 'satisfiesExver',
})
export class ExverSatisfiesPipe implements PipeTransform {
  constructor(private readonly exver: Exver) {}

  transform(versionUnderTest?: string, range?: string): boolean {
    return (
      !!versionUnderTest &&
      !!range &&
      this.exver.satisfies(versionUnderTest, range)
    )
  }
}

@Pipe({
  name: 'compareExver',
})
export class ExverComparesPipe implements PipeTransform {
  constructor(private readonly exver: Exver) {}

  transform(first: string, second: string): SemverResult {
    try {
      return this.exver.compareExver(first, second) as SemverResult
    } catch (e) {
      console.error(`exver comparison failed`, e, first, second)
      return 'comparison-impossible'
    }
  }
}
// left compared to right - if 1, version on left is higher; if 0, values the same; if -1, version on left is lower
type SemverResult = 0 | 1 | -1 | 'comparison-impossible'

@Pipe({
  name: 'displayExver',
})
export class ExverDisplayPipe implements PipeTransform {
  constructor() {}

  transform(version: string): string {
    return displayExver(version)
  }
}

export function displayExver(version: string): string {
  let res
  try {
    res = ExtendedVersion.parse(version)
  } catch (e) {
    try {
      res = VersionRange.parse(version)
    } catch (e) {
      throw e
    }
  }
  return res.toString()
}
