import { Pipe, PipeTransform } from '@angular/core'
import { toAcmeName } from 'src/app/utils/acme'

@Pipe({
  name: 'acme',
})
export class AcmePipe implements PipeTransform {
  transform(value: string | null = null): string {
    return toAcmeName(value)
  }
}
