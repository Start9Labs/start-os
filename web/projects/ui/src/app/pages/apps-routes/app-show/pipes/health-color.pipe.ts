import { Pipe, PipeTransform } from '@angular/core'
import { HealthCheckResult } from '../../../../../../../../../core/startos/bindings/HealthCheckResult'

@Pipe({
  name: 'healthColor',
})
export class HealthColorPipe implements PipeTransform {
  transform(val: HealthCheckResult['result']): string {
    switch (val) {
      case 'success':
        return 'success'
      case 'failure':
        return 'warning'
      case 'disabled':
        return 'dark'
      case 'starting':
      case 'loading':
        return 'primary'
    }
  }
}
