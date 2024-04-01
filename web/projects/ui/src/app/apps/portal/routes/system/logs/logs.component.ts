import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiTextfieldControllerModule } from '@taiga-ui/core'
import { TuiSelectModule } from '@taiga-ui/kit'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { RR } from 'src/app/services/api/api.types'
import { LogsComponent } from '../../../components/logs/logs.component'

@Component({
  template: `
    <div class="g-plaque"></div>
    <tui-select
      tuiTextfieldAppearance="unstyled"
      tuiTextfieldSize="m"
      [(ngModel)]="logs"
    >
      {{ subtitle }}
      <select tuiSelect [items]="items"></select>
    </tui-select>
    @switch (logs) {
      @case ('OS Logs') {
        <logs
          context="OS Logs"
          [followLogs]="followOS"
          [fetchLogs]="fetchOS"
        ></logs>
      }
      @case ('Kernel Logs') {
        <logs
          context="Kernel Logs"
          [followLogs]="followKernel"
          [fetchLogs]="fetchKernel"
        ></logs>
      }
      @case ('Tor Logs') {
        <logs
          context="Tor Logs"
          [followLogs]="followTor"
          [fetchLogs]="fetchTor"
        ></logs>
      }
    }
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-edged' },
  styles: [
    `
      tui-select {
        margin: -0.5rem 0 1rem;
      }

      logs {
        height: calc(100% - 4rem);
      }

      :host-context(tui-root._mobile) {
        --clip-path: none;
        height: 100%;
        margin: 0;
        padding: 1rem 1rem 0;
        border: 0.375rem solid transparent;
        border-top: 0;
      }
    `,
  ],
  imports: [
    FormsModule,
    TuiSelectModule,
    TuiTextfieldControllerModule,
    LogsComponent,
  ],
})
export default class SystemLogsComponent {
  private readonly api = inject(ApiService)
  readonly items = ['OS Logs', 'Kernel Logs', 'Tor Logs']
  logs = 'OS Logs'

  readonly followOS = async (params: RR.FollowServerLogsReq) =>
    this.api.followServerLogs(params)
  readonly fetchOS = async (params: RR.GetServerLogsReq) =>
    this.api.getServerLogs(params)

  readonly followKernel = async (params: RR.FollowServerLogsReq) =>
    this.api.followKernelLogs(params)
  readonly fetchKernel = async (params: RR.GetServerLogsReq) =>
    this.api.getKernelLogs(params)

  readonly followTor = async (params: RR.FollowServerLogsReq) =>
    this.api.followTorLogs(params)
  readonly fetchTor = async (params: RR.GetServerLogsReq) =>
    this.api.getTorLogs(params)

  get subtitle(): string {
    switch (this.logs) {
      case 'OS Logs':
        return 'Raw, unfiltered operating system logs'
      case 'Kernel Logs':
        return 'Diagnostic log stream for device drivers and other kernel processes'
      default:
        return 'Diagnostic log stream for the Tor daemon on StartOS'
    }
  }
}
