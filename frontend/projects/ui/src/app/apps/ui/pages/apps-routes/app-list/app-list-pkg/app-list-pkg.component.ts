import {
  ChangeDetectionStrategy,
  Component,
  Input,
  ViewChild,
} from '@angular/core'
import { LaunchMenuComponent } from 'src/app/components/launch-menu/launch-menu.component'
import { PackageMainStatus } from 'src/app/services/patch-db/data-model'
import { PkgInfo } from 'src/app/util/get-package-info'

@Component({
  selector: 'app-list-pkg',
  templateUrl: 'app-list-pkg.component.html',
  styleUrls: ['app-list-pkg.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListPkgComponent {
  @ViewChild('launchMenu') launchMenu!: LaunchMenuComponent

  @Input()
  pkg!: PkgInfo

  get status(): PackageMainStatus {
    return (
      this.pkg.entry.installed?.status.main.status || PackageMainStatus.Stopped
    )
  }

  openPopover(e: Event): void {
    e.stopPropagation()
    e.preventDefault()
    this.launchMenu.event = e
    this.launchMenu.isOpen = true
  }
}
