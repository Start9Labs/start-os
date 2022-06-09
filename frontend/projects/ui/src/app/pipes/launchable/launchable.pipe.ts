import { Pipe, PipeTransform } from '@angular/core'
import {
  InterfaceDef,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { ConfigService } from '../../services/config.service'

@Pipe({
  name: 'isLaunchable',
})
export class LaunchablePipe implements PipeTransform {
  constructor(private configService: ConfigService) {}

  transform(
    state: PackageState,
    status: PackageMainStatus,
    interfaces: Record<string, InterfaceDef>,
  ): boolean {
    return this.configService.isLaunchable(state, status, interfaces)
  }
}
