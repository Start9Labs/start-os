import { Pipe, PipeTransform } from '@angular/core'
import { HealthResult } from '@start9labs/shared'

@Pipe({
  name: 'healthColor',
})
export class HealthColorPipe implements PipeTransform {
  transform(val: HealthResult): string {
    switch (val) {
      case HealthResult.Success:
        return 'success'
      case HealthResult.Failure:
        return 'warning'
      case HealthResult.Disabled:
        return 'dark'
      case HealthResult.Starting:
      case HealthResult.Loading:
        return 'primary'
    }
  }
}
