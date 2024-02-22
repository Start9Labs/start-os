import { Inject, Injectable } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { InstalledPackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ConfigService } from './config.service'

@Injectable({
  providedIn: 'root',
})
export class UiLauncherService {
  constructor(
    @Inject(WINDOW) private readonly windowRef: Window,
    private readonly config: ConfigService,
  ) {}

  launch(interfaces: InstalledPackageDataEntry['service-interfaces']): void {
    this.windowRef.open(
      this.config.launchableAddress(interfaces),
      '_blank',
      'noreferrer',
    )
  }
}
