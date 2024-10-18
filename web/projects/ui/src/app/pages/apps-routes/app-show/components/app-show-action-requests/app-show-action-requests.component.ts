import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { ActionService } from 'src/app/services/action.service'
import { getDepDetails } from 'src/app/util/dep-info'

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
      dependency: {
        title: string
        icon: string
      } | null
    })[] = []
    const important: (T.ActionRequest & {
      actionName: string
      dependency: {
        title: string
        icon: string
      } | null
    })[] = []

    Object.values(this.pkg.requestedActions)
      .filter(r => r.active)
      .forEach(r => {
        const self = r.request.packageId === this.manifest.id
        const toReturn = {
          ...r.request,
          actionName: self
            ? this.pkg.actions[r.request.actionId].name
            : this.allPkgs[r.request.packageId]?.actions[r.request.actionId]
                .name || 'Unknown Action',
          dependency: self
            ? null
            : getDepDetails(this.pkg, this.allPkgs, r.request.packageId),
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
    const self = request.packageId === this.manifest.id
    this.actionService.present({
      pkgInfo: {
        id: request.packageId,
        title: self
          ? this.manifest.title
          : getDepDetails(this.pkg, this.allPkgs, request.packageId).title,
        mainStatus: self
          ? this.pkg.status.main
          : this.allPkgs[request.packageId].status.main,
        icon: self
          ? this.pkg.icon
          : getDepDetails(this.pkg, this.allPkgs, request.packageId).icon,
      },
      actionInfo: {
        id: request.actionId,
        metadata:
          request.packageId === this.manifest.id
            ? this.pkg.actions[request.actionId]
            : this.allPkgs[request.packageId].actions[request.actionId],
      },
      requestInfo: {
        request,
        dependentId:
          request.packageId === this.manifest.id ? undefined : this.manifest.id,
      },
    })
  }
}
