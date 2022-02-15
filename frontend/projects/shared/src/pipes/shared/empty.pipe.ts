import { Pipe, PipeTransform } from '@angular/core'
import { isEmptyObject } from '../../util/misc.util'

@Pipe({
  name: 'empty',
})
export class EmptyPipe implements PipeTransform {
  transform(val: object | [] = {}): boolean {
    if (Array.isArray(val)) return !val.length
    return isEmptyObject(val)
  }
}
