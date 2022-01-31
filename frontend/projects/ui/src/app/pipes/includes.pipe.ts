import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
    name: 'includes',
})
export class IncludesPipe implements PipeTransform {
  transform<T> (list: T[], val: T): boolean {
    return list.includes(val)
  }
}