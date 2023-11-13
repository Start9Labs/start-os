import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PackageMainStatus } from 'src/app/services/patch-db/data-model'
import { PkgInfo } from 'src/app/util/get-package-info'
import { UiLauncherService } from 'src/app/services/ui-launcher.service'

@Component({
  selector: 'app-list-pkg',
  templateUrl: 'app-list-pkg.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListPkgComponent {
  @Input()
  pkg!: PkgInfo

  constructor(private readonly launcherService: UiLauncherService) {}

  get status(): PackageMainStatus {
    return (
      this.pkg.entry.installed?.status.main.status || PackageMainStatus.Stopped
    )
  }

  launchUi(e: Event): void {
    e.stopPropagation()
    e.preventDefault()
    this.launcherService.launch(this.pkg.entry)
  }
}
