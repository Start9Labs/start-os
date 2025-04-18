import { inject, Pipe, PipeTransform } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { IST } from '@start9labs/start-sdk'

@Pipe({
  name: 'hint',
})
export class HintPipe implements PipeTransform {
  private readonly i18n = inject(i18nPipe)

  transform(spec: Exclude<IST.ValueSpec, IST.ValueSpecHidden>): string {
    const hint = []

    if (spec.description) {
      hint.push(spec.description)
    }

    if ('disabled' in spec && typeof spec.disabled === 'string') {
      hint.push(`${this.i18n.transform('Disabled')}: ${spec.disabled}`)
    }

    return hint.join('\n\n')
  }
}
