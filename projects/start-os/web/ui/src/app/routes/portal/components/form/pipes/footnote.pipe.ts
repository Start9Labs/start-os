import { Pipe, PipeTransform } from '@angular/core'
import { IST } from '@start9labs/start-sdk'

@Pipe({
  name: 'footnote',
})
export class FootnotePipe implements PipeTransform {
  transform(spec: Exclude<IST.ValueSpec, IST.ValueSpecHidden>): string | null {
    return 'footnote' in spec ? (spec.footnote ?? null) : null
  }
}
