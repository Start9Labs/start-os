import { Pipe, PipeTransform } from '@angular/core'
import { ConfigService } from '../../services/config.service'
import { T } from '@start9labs/start-sdk'

@Pipe({
  name: 'isLaunchable',
})
export class LaunchablePipe implements PipeTransform {
  constructor(private configService: ConfigService) {}

  transform(
    state: T.PackageState['state'],
    status: T.MainStatus['status'],
  ): boolean {
    return this.configService.isLaunchable(state, status)
  }
}
