import { inject, Injectable } from '@angular/core'
import { LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { from, switchMap } from 'rxjs'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TARGET, TARGET_CREATE } from '../modals/target.component'
import { BACKUP, BACKUP_OPTIONS } from '../modals/backup.component'

@Injectable({
  providedIn: 'root',
})
export class BackupsCreateService {
  private readonly loader = inject(LoadingService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)

  readonly handle = () => {
    this.dialogs
      .open<BackupTarget>(TARGET, TARGET_CREATE)
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
    const loader = this.loader.open('Beginning backup...').subscribe()

    await this.api
      .createBackup({ 'target-id': targetId, 'package-ids': pkgIds })
      .finally(() => loader.unsubscribe())
  }
}

const OPTIONS: Partial<TuiDialogOptions<{ btnText: string }>> = {
  ...BACKUP_OPTIONS,
  data: { btnText: 'Create Backup' },
}
