import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { getPkgId } from '@start9labs/shared'
import { LogsComponent } from 'src/app/routes/portal/components/logs/logs.component'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  template: '<logs [fetchLogs]="fetch" [followLogs]="follow" [context]="id" />',
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  styles: `
    :host {
      overflow: hidden;
    }

    :host-context(tui-root._mobile) {
      min-height: 0;
    }
  `,
  host: { class: 'g-subpage' },
  imports: [LogsComponent],
})
export default class ServiceLogsRoute {
  private readonly api = inject(ApiService)

  readonly id = getPkgId()

  readonly follow = async (params: RR.FollowServerLogsReq) =>
    this.api.followPackageLogs({ id: this.id, ...params })

  readonly fetch = async (params: RR.GetServerLogsReq) =>
    this.api.getPackageLogs({ id: this.id, ...params })
}
