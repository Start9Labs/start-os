import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { RR } from 'src/app/services/api/api.types'
import { LogsComponent } from 'src/app/routes/portal/components/logs/logs.component'

@Component({
  template: '<logs [fetchLogs]="fetch" [followLogs]="follow" [context]="id" />',
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  styles: [':host { height: 100%}'],
  imports: [LogsComponent],
})
export class ServiceLogsRoute {
  private readonly api = inject(ApiService)

  readonly id = getPkgId(inject(ActivatedRoute))

  readonly follow = async (params: RR.FollowServerLogsReq) =>
    this.api.followPackageLogs({ id: this.id, ...params })

  readonly fetch = async (params: RR.GetServerLogsReq) =>
    this.api.getPackageLogs({ id: this.id, ...params })
}
