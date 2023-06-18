import { Directive, HostListener } from '@angular/core'
import { LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TargetSelectPage } from '../modals/target-select/target-select.page'
import { BackupSelectPage } from '../modals/backup-select/backup-select.page'

@Directive({
  selector: '[backupCreate]',
})
export class BackupCreateDirective {
  constructor(
    private readonly loader: LoadingService,
    private readonly dialogs: TuiDialogService,
    private readonly embassyApi: ApiService,
  ) {}

  @HostListener('click')
  onClick() {
    this.presentModalTarget()
  }

  presentModalTarget() {
    this.dialogs
      .open<BackupTarget>(new PolymorpheusComponent(TargetSelectPage), {
        label: 'Select Backup Target',
        data: { type: 'create' },
      })
      .subscribe(({ id }) => {
        this.presentModalSelect(id)
      })
  }

  private presentModalSelect(targetId: string) {
    this.dialogs
      .open<string[]>(new PolymorpheusComponent(BackupSelectPage), {
        label: 'Select Services to Back Up',
        data: { btnText: 'Create Backup' },
      })
      .subscribe(pkgIds => {
        this.createBackup(targetId, pkgIds)
      })
  }

  private async createBackup(
    targetId: string,
    pkgIds: string[],
  ): Promise<void> {
    const loader = this.loader.open('Beginning backup...').subscribe()

    await this.embassyApi
      .createBackup({
        'target-id': targetId,
        'package-ids': pkgIds,
      })
      .finally(() => loader.unsubscribe())
  }
}
