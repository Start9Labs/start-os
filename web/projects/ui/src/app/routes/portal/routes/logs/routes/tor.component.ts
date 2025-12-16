import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { LogsComponent } from 'src/app/routes/portal/components/logs/logs.component'
import { LogsHeaderComponent } from 'src/app/routes/portal/routes/logs/components/header.component'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  template: `
    <logs-header [title]="'Tor Logs' | i18n">
      {{ 'Diagnostics for the Tor daemon on this server' | i18n }}
    </logs-header>
    <logs context="tor" [followLogs]="follow" [fetchLogs]="fetch" />
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
export default class SystemTorComponent {
  private readonly api = inject(ApiService)

  protected readonly follow = (params: RR.FollowServerLogsReq) =>
    this.api.followTorLogs(params)

  protected readonly fetch = (params: RR.GetServerLogsReq) =>
    this.api.getTorLogs(params)
}
