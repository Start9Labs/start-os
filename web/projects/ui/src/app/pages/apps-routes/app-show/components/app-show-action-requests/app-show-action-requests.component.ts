import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { ActionService } from 'src/app/services/action.service'
import { DependencyInfo } from 'src/app/pages/apps-routes/app-show/app-show.page'
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

  @Input()
  dep?: DependencyInfo

  pkgId!: string

  ngOnInit() {
    this.pkgId = this.dep ? this.dep?.id : this.manifest.id
  }

  get actionRequests() {
    const reqs: {
      [key: string]: (T.ActionRequest & {
        actionName: string
      })[]
    } = {}
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
        if (!reqs[r.request.packageId]) {
          reqs[r.request.packageId] = []
        }
        reqs[r.request.packageId].push(toReturn)
      })
    return reqs
  }
  constructor(private readonly actionService: ActionService) {}

  async handleAction(request: T.ActionRequest, e: Event) {
    e.stopPropagation()
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
