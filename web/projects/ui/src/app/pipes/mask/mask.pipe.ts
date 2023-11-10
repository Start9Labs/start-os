import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'mask',
})
export class MaskPipe implements PipeTransform {
  transform(val: string, max?: number): string {
    const length = max ? Math.min(max, val.length) : val.length
    return '●'.repeat(length)
  }
}
