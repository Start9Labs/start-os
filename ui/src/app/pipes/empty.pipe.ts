import { Pipe, PipeTransform } from '@angular/core'
import { isEmptyObject } from '../util/misc.util'

@Pipe({
  name: 'empty',
})
export class EmptyPipe implements PipeTransform {
  transform (obj: { }): boolean {
    return isEmptyObject(obj)
  }
}