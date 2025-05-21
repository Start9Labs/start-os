import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { LogsComponent } from 'src/app/routes/portal/components/logs/logs.component'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

import { LogsHeaderComponent } from '../components/header.component'

@Component({
  standalone: true,
  template: `
    <logs-header [title]="'Kernel Logs' | i18n">
      {{ 'Diagnostics for drivers and other kernel processes' | i18n }}
    </logs-header>
    <logs context="kernel" [followLogs]="follow" [fetchLogs]="fetch" />
  `,
  styles: `
    :host {
      padding: 1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [LogsComponent, LogsHeaderComponent, i18nPipe],
  host: { class: 'g-page' },
})
export default class SystemKernelComponent {
  private readonly api = inject(ApiService)

  protected readonly follow = (params: RR.FollowServerLogsReq) =>
    this.api.followKernelLogs(params)

  protected readonly fetch = (params: RR.GetServerLogsReq) =>
    this.api.getKernelLogs(params)

  log = {
    title: 'Kernel Logs',
    subtitle: 'Diagnostics for drivers and other kernel processes',
    icon: '@tui.square-chevron-right',
  }
}
