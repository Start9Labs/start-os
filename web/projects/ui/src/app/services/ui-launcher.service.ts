import { Inject, Injectable } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ConfigService } from './config.service'

@Injectable({
  providedIn: 'root',
})
export class UiLauncherService {
  constructor(
    @Inject(WINDOW) private readonly windowRef: Window,
    private readonly config: ConfigService,
  ) {}

  launch(interfaces: PackageDataEntry['serviceInterfaces']): void {
    this.windowRef.open(
      this.config.launchableAddress(interfaces),
      '_blank',
      'noreferrer',
    )
  }
}
