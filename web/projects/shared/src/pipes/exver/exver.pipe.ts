import { Pipe, PipeTransform } from '@angular/core'
import { Exver } from '../../services/exver.service'

@Pipe({
  name: 'satisfiesExver',
  standalone: false,
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
  standalone: false,
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
