import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { LogsComponentModule } from 'src/app/common/logs/logs.component.module'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { RR } from 'src/app/services/api/api.types'
import { updateTab } from '../utils/update-tab'

@Component({
  template: '<logs [fetchLogs]="fetch" [followLogs]="follow" [context]="id" />',
  styles: [
    `
      logs {
        display: block;
        height: calc(100% - 9rem);
        min-height: 20rem;
        margin-bottom: 5rem;

        ::ng-deep ion-header {
          display: none;
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [LogsComponentModule],
})
export class ServiceLogsRoute {
  private readonly api = inject(ApiService)

  readonly id = getPkgId(inject(ActivatedRoute))

  readonly follow = async (params: RR.FollowServerLogsReq) =>
    this.api.followPackageLogs({ id: this.id, ...params })

  readonly fetch = async (params: RR.GetServerLogsReq) =>
    this.api.getPackageLogs({ id: this.id, ...params })

  constructor() {
    updateTab('/logs')
  }
}
