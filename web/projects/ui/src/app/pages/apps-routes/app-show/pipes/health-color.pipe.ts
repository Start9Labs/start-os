import { Pipe, PipeTransform } from '@angular/core'
import { T } from '@start9labs/start-sdk'

@Pipe({
  name: 'healthColor',
})
export class HealthColorPipe implements PipeTransform {
  transform(val: T.HealthCheckResult['result']): string {
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
