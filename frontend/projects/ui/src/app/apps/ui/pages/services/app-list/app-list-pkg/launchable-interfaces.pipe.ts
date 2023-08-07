import { Pipe, PipeTransform } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { InstalledPackageInfo } from 'src/app/services/patch-db/data-model'

@Pipe({
  name: 'launchableInterfaces',
})
export class LaunchableInterfacesPipe implements PipeTransform {
  constructor(private readonly config: ConfigService) {}

  transform(
    interfaceInfo: InstalledPackageInfo['interfaceInfo'],
  ): LaunchableInterface[] {
    return Object.values(interfaceInfo)
      .filter(info => info.type === 'ui')
      .map(info => ({
        name: info.name,
        address: this.config.launchableAddress(info),
      }))
  }
}

export type LaunchableInterface = {
  name: string
  address: string
}
