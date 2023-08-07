import { DOCUMENT } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
  ViewChild,
} from '@angular/core'
import { InstalledPackageInfo } from 'src/app/services/patch-db/data-model'
import { LaunchableInterface } from '../launchable-interfaces.pipe'

@Component({
  selector: 'launch-menu',
  templateUrl: 'launch-menu.component.html',
  styleUrls: ['launch-menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LaunchMenuComponent {
  @ViewChild('popover') popover!: HTMLIonPopoverElement

  @Input({ required: true })
  launchableInterfaces!: LaunchableInterface[]

  set isOpen(open: boolean) {
    this.popover.isOpen = open
  }

  set event(event: Event) {
    this.popover.event = event
  }

  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  launchUI(address: string) {
    this.document.defaultView?.open(address, '_blank', 'noreferrer')
    this.popover.isOpen = false
  }
}
