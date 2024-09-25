import { Pipe, PipeTransform } from '@angular/core'
import { IST } from '@start9labs/start-sdk'
import { KeyValue } from '@angular/common'

@Pipe({
  name: 'filterHidden',
})
export class FilterHiddenPipe implements PipeTransform {
  transform(value: KeyValue<string, IST.ValueSpec>[]) {
    return value.filter(x => x.value.type !== 'hidden') as KeyValue<
      string,
      Exclude<IST.ValueSpec, IST.ValueSpecHidden>
    >[]
  }
}
