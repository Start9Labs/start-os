import { DatePipe, KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  DialogService,
  ErrorService,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { verifyPassword } from 'src/app/utils/verify-password'
import { BackupContext } from './backup.types'
import { RECOVER } from './recover.component'

@Component({
  template: `
    <table [appTable]="['Hostname', 'StartOS Version', 'Created', null]">
      @for (server of target.entry.startOs | keyvalue; track $index) {
        <tr>
          <td class="name">{{ server.value.hostname }}.local</td>
          <td>{{ server.value.version }}</td>
          <td>{{ server.value.timestamp | date: 'medium' }}</td>
          <td>
            <button
              tuiButton
              size="s"
              (click)="onClick(server.key, server.value)"
            >
              Select
            </button>
          </td>
        </tr>
      }
    </table>
  `,
  styles: `
    td:last-child {
      text-align: right;
    }

    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: 1fr auto;
      }

      .name {
        color: var(--tui-text-primary);
        font: var(--tui-typography-body-m);
        font-weight: bold;
      }

      td:last-child {
        grid-area: 1 / 2 / 4 / 2;
        align-self: center;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [KeyValuePipe, DatePipe, TuiButton, TableComponent],
})
export class BackupRestoreComponent {
  private readonly dialog = inject(DialogService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly context = injectContext<BackupContext>()

  readonly target = this.context.data

  onClick(serverId: string, { passwordHash }: StartOSDiskInfo) {
    this.dialog
      .openPrompt<string>({
        label: 'Password required',
        data: {
          message:
            'Enter the master password that was used to encrypt this backup. On the next screen, you will select the individual services you want to restore.',
          label: 'Master Password',
          placeholder: 'Enter master password',
          useMask: true,
        },
      })
      .pipe(verifyPassword(passwordHash, e => this.errorService.handleError(e)))
      .subscribe(async password => await this.restore(serverId, password))
  }

  private async restore(serverId: string, password: string): Promise<void> {
    const loader = this.loader.open('Decrypting drive').subscribe()
    const params = { targetId: this.target.id, serverId, password }

    try {
      const backupInfo = await this.api.getBackupInfo(params)
      const data = {
        targetId: this.target.id,
        serverId,
        backupInfo,
        password,
      }

      this.context.$implicit.complete()
      this.dialog
        .openComponent(RECOVER, { label: 'Select services', data })
        .subscribe()
    } finally {
      loader.unsubscribe()
    }
  }
}

export const BACKUP_RESTORE = new PolymorpheusComponent(BackupRestoreComponent)
