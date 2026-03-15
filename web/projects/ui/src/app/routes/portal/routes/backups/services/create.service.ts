import { inject, Injectable } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { from, switchMap } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BACKUP, BACKUP_OPTIONS } from '../modals/backup.component'
import { TARGET, TARGET_CREATE } from '../modals/target.component'

@Injectable({
  providedIn: 'root',
})
export class BackupsCreateService {
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)

  readonly handle = () => {
    this.dialogs
      .open<T.BackupTarget & { id: string }>(TARGET, TARGET_CREATE)
      .pipe(
        switchMap(({ id }) =>
          this.dialogs
            .open<string[]>(BACKUP, OPTIONS)
            .pipe(switchMap(ids => from(this.createBackup(id, ids)))),
        ),
      )
      .subscribe()
  }

  private async createBackup(
    targetId: string,
    pkgIds: string[],
  ): Promise<void> {
    const loader = this.loader.open('Beginning backup').subscribe()

    await this.api
      .createBackup({ targetId, packageIds: pkgIds })
      .finally(() => loader.unsubscribe())
  }
}

const OPTIONS: Partial<TuiDialogOptions<{ btnText: string }>> = {
  ...BACKUP_OPTIONS,
  data: { btnText: 'Create Backup' },
}
