import { DatePipe, KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  DialogService,
  ErrorService,
  i18nPipe,
  LoadingService,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { verifyPassword } from 'src/app/utils/verify-password'
import { BackupContext } from './backup.types'
import { RECOVER } from './recover.component'

@Component({
  template: `
    @for (server of target.entry.startOs | keyvalue; track $index) {
      <button
        tuiCell
        class="g-stretch"
        (click)="onClick(server.key, server.value)"
      >
        <span tuiTitle>
          <span tuiSubtitle>
            <b>{{ 'Local Hostname' | i18n }}</b>
            : {{ server.value.hostname }}.local
          </span>
          <span tuiSubtitle>
            <b>{{ 'StartOS Version' | i18n }}</b>
            : {{ server.value.version }}
          </span>
          <span tuiSubtitle>
            <b>{{ 'Created' | i18n }}</b>
            : {{ server.value.timestamp | date: 'medium' }}
          </span>
        </span>
      </button>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [KeyValuePipe, DatePipe, TuiCell, TuiTitle, i18nPipe],
})
export class BackupRestoreComponent {
  private readonly dialog = inject(DialogService)
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly context = injectContext<BackupContext>()

  readonly target = this.context.data

  onClick(serverId: string, { passwordHash }: StartOSDiskInfo) {
    this.dialog
      .openPrompt<string>({
        label: 'Password Required',
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
        .openComponent(RECOVER, { label: 'Select services to restore', data })
        .subscribe()
    } finally {
      loader.unsubscribe()
    }
  }
}

export const BACKUP_RESTORE = new PolymorpheusComponent(BackupRestoreComponent)
