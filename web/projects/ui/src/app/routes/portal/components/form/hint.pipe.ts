import { Pipe, PipeTransform } from '@angular/core'
import { CT } from '@start9labs/start-sdk'

@Pipe({
  name: 'hint',
})
export class HintPipe implements PipeTransform {
  transform(spec: CT.ValueSpec): string {
    const hint = []

    if (spec.description) {
      hint.push(spec.description)
    }

    if ('disabled' in spec && typeof spec.disabled === 'string') {
      hint.push(`Disabled: ${spec.disabled}`)
    }

    return hint.join('\n\n')
  }
}
