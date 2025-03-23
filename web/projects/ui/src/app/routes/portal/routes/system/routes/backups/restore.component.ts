import { DatePipe, KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  ErrorService,
  LoadingService,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PROMPT } from 'src/app/routes/portal/modals/prompt.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { verifyPassword } from 'src/app/utils/verify-password'
import { RESTORE_OPTIONS } from './backup.const'
import { BackupContext } from './backup.types'
import { RECOVER } from './recover.component'

@Component({
  standalone: true,
  template: `
    @for (server of target.entry.startOs | keyvalue; track $index) {
      <button tuiCell (click)="onClick(server.key, server.value)">
        <span tuiTitle>
          <span tuiSubtitle>
            <b>Local Hostname</b>
            : {{ server.value.hostname }}.local
          </span>
          <span tuiSubtitle>
            <b>StartOS Version</b>
            : {{ server.value.version }}
          </span>
          <span tuiSubtitle>
            <b>Created</b>
            : {{ server.value.timestamp | date: 'medium' }}
          </span>
        </span>
      </button>
    }
  `,
  styles: `
    [tuiCell] {
      width: stretch;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [KeyValuePipe, DatePipe, TuiCell, TuiTitle],
})
export class BackupRestoreComponent {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly context = injectContext<BackupContext>()

  readonly target = this.context.data

  onClick(serverId: string, { passwordHash }: StartOSDiskInfo) {
    this.dialogs
      .open<string>(PROMPT, RESTORE_OPTIONS)
      .pipe(verifyPassword(passwordHash, e => this.errorService.handleError(e)))
      .subscribe(async password => await this.restore(serverId, password))
  }

  private async restore(serverId: string, password: string): Promise<void> {
    const loader = this.loader.open('Decrypting drive...').subscribe()
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
      this.dialogs
        .open(RECOVER, { label: 'Select Services to Restore', data })
        .subscribe()
    } finally {
      loader.unsubscribe()
    }
  }
}

export const BACKUP_RESTORE = new PolymorpheusComponent(BackupRestoreComponent)
