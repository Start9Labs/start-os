import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'typeof',
})
export class TypeofPipe implements PipeTransform {
  transform (value: any): any {
    if (value === null) {
      return 'null'
    } else if (Array.isArray(value)) {
      return 'array'
    }

    return typeof value
  }
}