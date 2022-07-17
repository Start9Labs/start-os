import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'mask',
})
export class MaskPipe implements PipeTransform {
  transform(val: string): string {
    return val && '●'.repeat(val.length)
  }
}
