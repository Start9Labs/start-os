import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
    name: 'includes',
})
export class IncludesPipe implements PipeTransform {
  transform<T> (set: T[], val: T): boolean {
    return set.includes(val)
  }
}