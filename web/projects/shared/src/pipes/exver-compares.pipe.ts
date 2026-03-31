import { inject, Pipe, PipeTransform } from '@angular/core'
import { Exver } from '../services/exver.service'

@Pipe({
  name: 'compareExver',
})
export class ExverComparesPipe implements PipeTransform {
  private readonly exver = inject(Exver)

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
