import { Inject, Injectable } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ConfigService } from './config.service'
import { T } from '@start9labs/start-sdk'

@Injectable({
  providedIn: 'root',
})
export class UiLauncherService {
  constructor(
    @Inject(WINDOW) private readonly windowRef: Window,
    private readonly config: ConfigService,
  ) {}

  launch(
    interfaces: PackageDataEntry['serviceInterfaces'],
    hosts: PackageDataEntry['hosts'],
  ): void {
    this.windowRef.open(
      this.config.launchableAddress(interfaces, hosts),
      '_blank',
      'noreferrer',
    )
  }
}
