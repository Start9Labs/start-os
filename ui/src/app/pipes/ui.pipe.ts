import { Pipe, PipeTransform } from '@angular/core'
import { InterfaceDef, PackageMainStatus, PackageState } from '../services/patch-db/data-model'
import { ConfigService, hasUi } from '../services/config.service'

@Pipe({
  name: 'hasUi',
})
export class HasUiPipe implements PipeTransform {

  transform (interfaces: { [id: string]: InterfaceDef }): boolean {
    return hasUi(interfaces)
  }
}

@Pipe({
  name: 'isLaunchable',
})
export class LaunchablePipe implements PipeTransform {

  constructor (private configService: ConfigService) { }

  transform (state: PackageState, status: PackageMainStatus, interfaces: { [id: string]: InterfaceDef }): boolean {
    return this.configService.isLaunchable(state, status, interfaces)
  }
}
