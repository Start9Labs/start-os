import {
  ChangeDetectionStrategy,
  Component,
  HostListener,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { ActionService } from 'src/app/services/action.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getDepDetails } from 'src/app/utils/dep-info'
import { getManifest } from 'src/app/utils/get-package-data'

export type ActionRequest = T.ActionRequest & {
  actionName: string
  dependency: {
    title: string
    icon: string
  } | null
}

@Component({
  standalone: true,
  selector: 'button[actionRequest]',
  template: `
    <tui-icon class="g-warning" [icon]="icon" />
    <span tuiTitle>
      <strong>{{ actionRequest.actionName }}</strong>
      @if (actionRequest.dependency) {
        <span tuiSubtitle>
          <strong>Service:</strong>
          <img
            alt=""
            [src]="actionRequest.dependency.icon"
            [style.width.rem]="1"
          />
          {{ actionRequest.dependency.title }}
        </span>
      }
      <span tuiSubtitle>
        {{ actionRequest.reason || 'no reason provided' }}
      </span>
    </span>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, TuiTitle],
  hostDirectives: [TuiCell],
})
export class ServiceActionRequestComponent {
  private readonly actionService = inject(ActionService)

  @Input({ required: true })
  actionRequest!: ActionRequest

  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  allPkgs!: Record<string, PackageDataEntry>

  get icon(): string {
    return this.actionRequest.severity === 'critical'
      ? '@tui.triangle-alert'
      : '@tui.play'
  }

  @HostListener('click')
  async handleAction() {
    const { id, title } = getManifest(this.pkg)
    const { actionId, packageId } = this.actionRequest
    const details = getDepDetails(this.pkg, this.allPkgs, packageId)
    const self = packageId === id

    this.actionService.present({
      pkgInfo: {
        id: packageId,
        title: self ? title : details.title,
        mainStatus: self
          ? this.pkg.status.main
          : this.allPkgs[packageId].status.main,
        icon: self ? this.pkg.icon : details.icon,
      },
      actionInfo: {
        id: actionId,
        metadata: self
          ? this.pkg.actions[actionId]
          : this.allPkgs[packageId].actions[actionId],
      },
      requestInfo: {
        request: this.actionRequest,
        dependentId: self ? undefined : id,
      },
    })
  }
}
