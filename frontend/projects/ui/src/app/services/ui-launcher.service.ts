import { Inject, Injectable } from '@angular/core'
import { DOCUMENT } from '@angular/common'
import { PackageDataEntry } from '@start9labs/shared'
import { ConfigService } from './config.service'

@Injectable({
  providedIn: 'root',
})
export class UiLauncherService {
  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly config: ConfigService,
  ) {}

  launch(pkg: PackageDataEntry): void {
    this.document.defaultView.open(
      this.config.launchableURL(pkg),
      '_blank',
      'noreferrer',
    )
  }
}
