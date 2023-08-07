import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
  ViewChild,
} from '@angular/core'
import { LaunchMenuComponent } from './launch-menu/launch-menu.component'
import { PackageMainStatus } from 'src/app/services/patch-db/data-model'
import { PkgInfo } from 'src/app/types/pkg-info'
import { DOCUMENT } from '@angular/common'

@Component({
  selector: 'app-list-pkg',
  templateUrl: 'app-list-pkg.component.html',
  styleUrls: ['app-list-pkg.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListPkgComponent {
  @ViewChild('launchMenu') launchMenu!: LaunchMenuComponent

  @Input({ required: true })
  pkg!: PkgInfo

  get status(): PackageMainStatus {
    return (
      this.pkg.entry.installed?.status.main.status || PackageMainStatus.Stopped
    )
  }

  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  openPopover(e: Event): void {
    e.stopPropagation()
    e.preventDefault()
    this.launchMenu.event = e
    this.launchMenu.isOpen = true
  }

  launchUI(address: string, e: Event) {
    e.stopPropagation()
    e.preventDefault()
    this.document.defaultView?.open(address, '_blank', 'noreferrer')
  }
}
