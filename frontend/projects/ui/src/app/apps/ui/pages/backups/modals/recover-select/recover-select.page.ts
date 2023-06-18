import { Component, Inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext } from '@taiga-ui/core'
import { BackupInfo } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import { AppRecoverOption } from './to-options.pipe'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { take } from 'rxjs'

export interface RecoverData {
  targetId: string
  backupInfo: BackupInfo
  password: string
}

@Component({
  selector: 'recover-select',
  templateUrl: './recover-select.page.html',
  styleUrls: ['./recover-select.page.scss'],
})
export class RecoverSelectPage {
  readonly packageData$ = this.patch.watch$('package-data').pipe(take(1))

  hasSelection = false

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<void, RecoverData>,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  get backupInfo(): BackupInfo {
    return this.context.data.backupInfo
  }

  handleChange(options: AppRecoverOption[]) {
    this.hasSelection = options.some(o => o.checked)
  }

  async restore(options: AppRecoverOption[]): Promise<void> {
    const ids = options.filter(({ checked }) => !!checked).map(({ id }) => id)
    const loader = this.loader.open('Initializing...').subscribe()

    try {
      await this.embassyApi.restorePackages({
        ids,
        'target-id': this.context.data.targetId,
        password: this.context.data.password,
      })

      this.context.completeWith(undefined)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
