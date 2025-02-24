import {
  ChangeDetectionStrategy,
  Component,
  HostListener,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiTitle } from '@taiga-ui/core'
import { TuiFade } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { ActionService } from 'src/app/services/action.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  standalone: true,
  selector: 'button[actionRequest]',
  template: `
    <span tuiTitle>
      <strong tuiFade><ng-content /></strong>
      <span tuiSubtitle>
        {{ actionRequest.reason || 'No reason provided' }}
      </span>
    </span>
  `,
  styles: `
    :host {
      width: 100%;
      margin: 0 -1rem;
    }

    strong {
      white-space: nowrap;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiTitle, TuiFade],
  hostDirectives: [TuiCell],
})
export class ServiceActionRequestComponent {
  private readonly actionService = inject(ActionService)

  @Input({ required: true })
  actionRequest!: T.ActionRequest

  @Input({ required: true })
  pkg!: PackageDataEntry

  @HostListener('click')
  async handleAction() {
    const { title } = getManifest(this.pkg)
    const { actionId, packageId } = this.actionRequest

    this.actionService.present({
      pkgInfo: {
        id: packageId,
        title,
        mainStatus: this.pkg.status.main,
        icon: this.pkg.icon,
      },
      actionInfo: {
        id: actionId,
        metadata: this.pkg.actions[actionId],
      },
      requestInfo: this.actionRequest,
    })
  }
}
