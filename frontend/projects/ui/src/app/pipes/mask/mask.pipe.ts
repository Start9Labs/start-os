import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'mask',
})
export class MaskPipe implements PipeTransform {
  transform(val: string, max = 16): string {
    return val && '●'.repeat(Math.min(val.length, max))
  }
}
