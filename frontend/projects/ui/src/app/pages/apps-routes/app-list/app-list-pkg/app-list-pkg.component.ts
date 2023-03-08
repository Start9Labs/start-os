import {
  ChangeDetectionStrategy,
  Component,
  Input,
  ViewChild,
} from '@angular/core'
import { PackageMainStatus } from 'src/app/services/patch-db/data-model'
import { PkgInfo } from 'src/app/util/get-package-info'

@Component({
  selector: 'app-list-pkg',
  templateUrl: 'app-list-pkg.component.html',
  styleUrls: ['app-list-pkg.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListPkgComponent {
  @ViewChild('popover') popover!: HTMLIonPopoverElement

  @Input()
  pkg!: PkgInfo

  isPopoverOpen = false

  get status(): PackageMainStatus {
    return (
      this.pkg.entry.installed?.status.main.status || PackageMainStatus.Stopped
    )
  }

  openPopover(e: Event): void {
    e.stopPropagation()
    e.preventDefault()
    this.popover.event = e
    this.isPopoverOpen = true
  }
}
