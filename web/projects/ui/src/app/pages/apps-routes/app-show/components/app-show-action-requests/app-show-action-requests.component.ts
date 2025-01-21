import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { ActionService } from 'src/app/services/action.service'

@Component({
  selector: 'app-show-action-requests',
  templateUrl: './app-show-action-requests.component.html',
  styleUrls: ['./app-show-action-requests.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowActionRequestsComponent {
  @Input()
  allPkgs!: Record<string, T.PackageDataEntry>

  @Input()
  pkg!: T.PackageDataEntry

  @Input()
  manifest!: T.Manifest

  get actionRequests() {
    const critical: (T.ActionRequest & {
      actionName: string
    })[] = []
    const important: (T.ActionRequest & {
      actionName: string
    })[] = []

    Object.values(this.pkg.requestedActions)
      .filter(r => r.active && r.request.packageId === this.manifest.id)
      .forEach(r => {
        const toReturn = {
          ...r.request,
          actionName: this.pkg.actions[r.request.actionId].name,
        }

        if (r.request.severity === 'critical') {
          critical.push(toReturn)
        } else {
          important.push(toReturn)
        }
      })

    return { critical, important }
  }

  constructor(private readonly actionService: ActionService) {}

  async handleAction(request: T.ActionRequest) {
    this.actionService.present({
      pkgInfo: {
        id: request.packageId,
        title: this.manifest.title,
        mainStatus: this.pkg.status.main,
        icon: this.pkg.icon,
      },
      actionInfo: {
        id: request.actionId,
        metadata: this.pkg.actions[request.actionId],
      },
      requestInfo: {
        request,
      },
    })
  }
}
