import { Pipe, PipeTransform } from '@angular/core'
import { ValueSpec } from '@start9labs/start-sdk/lib/config/configTypes'

@Pipe({
  name: 'hint',
})
export class HintPipe implements PipeTransform {
  transform(spec: ValueSpec): string {
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
