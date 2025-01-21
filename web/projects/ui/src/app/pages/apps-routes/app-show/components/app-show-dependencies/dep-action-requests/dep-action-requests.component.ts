import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { ActionService } from 'src/app/services/action.service'
import { getDepDetails } from 'src/app/util/dep-info'
import { DependencyInfo } from 'src/app/pages/apps-routes/app-show/app-show.page'

@Component({
  selector: 'dep-action-requests',
  templateUrl: './dep-action-requests.component.html',
  styleUrls: ['./dep-action-requests.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DepActionRequestsComponent {
  @Input()
  allPkgs!: Record<string, T.PackageDataEntry>

  @Input()
  pkg!: T.PackageDataEntry

  @Input()
  dep!: DependencyInfo

  @Input()
  pkgId!: string

  get actionRequests() {
    const reqs: {
      [key: string]: (T.ActionRequest & {
        actionName: string
      })[]
    } = {}

    Object.values(this.pkg.requestedActions)
      .filter(r => r.active)
      .forEach(r => {
        const toReturn = {
          ...r.request,
          actionName:
            this.allPkgs[r.request.packageId]?.actions[r.request.actionId]
              .name || 'Unknown Action',
        }
        if (!reqs[r.request.packageId]) {
          reqs[r.request.packageId] = []
        }
        reqs[r.request.packageId].push(toReturn)
      })

    return reqs
  }

  constructor(private readonly actionService: ActionService) {}

  async handleAction(request: T.ActionRequest) {
    this.actionService.present({
      pkgInfo: {
        id: request.packageId,
        title: getDepDetails(this.pkg, this.allPkgs, request.packageId).title,
        mainStatus: this.allPkgs[request.packageId].status.main,
        icon: getDepDetails(this.pkg, this.allPkgs, request.packageId).icon,
      },
      actionInfo: {
        id: request.actionId,
        metadata: this.allPkgs[request.packageId].actions[request.actionId],
      },
      requestInfo: {
        request,
        dependentId: this.pkgId,
      },
    })
  }
}
