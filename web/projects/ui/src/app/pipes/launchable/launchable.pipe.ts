import { Pipe, PipeTransform } from '@angular/core'
import { ConfigService } from '../../services/config.service'
import { PackageState } from '../../../../../../../core/startos/bindings/PackageState'
import { MainStatus } from '../../../../../../../core/startos/bindings/MainStatus'

@Pipe({
  name: 'isLaunchable',
})
export class LaunchablePipe implements PipeTransform {
  constructor(private configService: ConfigService) {}

  transform(
    state: PackageState['state'],
    status: MainStatus['status'],
  ): boolean {
    return this.configService.isLaunchable(state, status)
  }
}
