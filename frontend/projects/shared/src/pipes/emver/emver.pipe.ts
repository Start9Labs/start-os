import { Pipe, PipeTransform } from '@angular/core'
import { Emver } from '../../services/emver.service'

@Pipe({
  name: 'satisfiesEmver',
})
export class EmverSatisfiesPipe implements PipeTransform {
  constructor(private readonly emver: Emver) {}

  transform(versionUnderTest?: string, range?: string): boolean {
    return (
      !!versionUnderTest &&
      !!range &&
      this.emver.satisfies(versionUnderTest, range)
    )
  }
}

@Pipe({
  name: 'compareEmver',
})
export class EmverComparesPipe implements PipeTransform {
  constructor(private readonly emver: Emver) {}

  transform(first: string, second: string): SemverResult {
    try {
      return this.emver.compare(first, second) as SemverResult
    } catch (e) {
      console.error(`emver comparison failed`, e, first, second)
      return 'comparison-impossible'
    }
  }
}
// left compared to right - if 1, version on left is higher; if 0, values the same; if -1, version on left is lower
type SemverResult = 0 | 1 | -1 | 'comparison-impossible'

@Pipe({
  name: 'displayEmver',
})
export class EmverDisplayPipe implements PipeTransform {
  constructor() {}

  transform(version: string): string {
    return displayEmver(version)
  }
}

export function displayEmver(version: string): string {
  const vs = version.split('.')
  if (vs.length === 4) return `${vs[0]}.${vs[1]}.${vs[2]}~${vs[3]}`
  return version
}
