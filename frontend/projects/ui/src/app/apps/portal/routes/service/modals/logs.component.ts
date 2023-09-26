import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { RR } from 'src/app/services/api/api.types'
import { LogsComponentModule } from 'src/app/common/logs/logs.component.module'

@Component({
  template:
    '<logs [fetchLogs]="fetch" [followLogs]="follow" [context]="id"></logs>',
  styles: [
    `
      logs {
        display: block;
        height: 60vh;
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
export class ServiceLogsModal {
  private readonly api = inject(ApiService)

  readonly id = inject<{ data: string }>(POLYMORPHEUS_CONTEXT).data

  readonly follow = async (params: RR.FollowServerLogsReq) =>
    this.api.followPackageLogs({ id: this.id, ...params })

  readonly fetch = async (params: RR.GetServerLogsReq) =>
    this.api.getPackageLogs({ id: this.id, ...params })
}
