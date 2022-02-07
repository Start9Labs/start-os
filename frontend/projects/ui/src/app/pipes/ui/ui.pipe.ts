import { Pipe, PipeTransform } from '@angular/core'
import { InterfaceDef } from '@start9labs/shared'
import { hasUi } from '../../services/config.service'

@Pipe({
  name: 'hasUi',
})
export class UiPipe implements PipeTransform {
  transform(interfaces: Record<string, InterfaceDef>): boolean {
    return hasUi(interfaces)
  }
}
