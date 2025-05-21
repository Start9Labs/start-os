import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { LogsComponent } from 'src/app/routes/portal/components/logs/logs.component'
import { LogsHeaderComponent } from 'src/app/routes/portal/routes/logs/components/header.component'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  standalone: true,
  template: `
    <logs-header [title]="'Tor Logs' | i18n">
      {{ 'Diagnostic logs for the Tor daemon on StartOS' | i18n }}
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
export default class SystemOSComponent {
  private readonly api = inject(ApiService)

  protected readonly follow = (params: RR.FollowServerLogsReq) =>
    this.api.followServerLogs(params)

  protected readonly fetch = (params: RR.GetServerLogsReq) =>
    this.api.getServerLogs(params)

  log = {
    title: 'Kernel Logs',
    subtitle: 'Diagnostics for drivers and other kernel processes',
    icon: '@tui.square-chevron-right',
  }
}
