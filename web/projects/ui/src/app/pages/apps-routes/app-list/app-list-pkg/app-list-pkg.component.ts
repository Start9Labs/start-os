import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PkgInfo } from 'src/app/util/get-package-info'
import { UiLauncherService } from 'src/app/services/ui-launcher.service'
import { T } from '@start9labs/start-sdk'

@Component({
  selector: 'app-list-pkg',
  templateUrl: 'app-list-pkg.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListPkgComponent {
  @Input()
  pkg!: PkgInfo

  constructor(private readonly launcherService: UiLauncherService) {}

  get pkgMainStatus(): T.MainStatus['main'] {
    return this.pkg.entry.status.main
  }

  get sigtermTimeout(): string | null {
    return this.pkgMainStatus === 'stopping' ? '30s' : null // @dr-bonez TODO
  }

  launchUi(
    e: Event,
    interfaces: PackageDataEntry['serviceInterfaces'],
    hosts: PackageDataEntry['hosts'],
  ): void {
    e.stopPropagation()
    e.preventDefault()
    this.launcherService.launch(interfaces, hosts)
  }
}
