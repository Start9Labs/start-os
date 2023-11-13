import { Component, Inject } from '@angular/core'
import { TuiDialogContext, TuiDialogService } from '@taiga-ui/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupJob, BackupTarget } from 'src/app/services/api/api.types'
import { TargetSelectPage } from '../../../modals/target-select/target-select.page'
import { BackupSelectPage } from '../../../modals/backup-select/backup-select.page'
import { BackupJobBuilder } from './job-builder'

@Component({
  selector: 'edit-job',
  templateUrl: './edit-job.component.html',
  styleUrls: ['./edit-job.component.scss'],
})
export class EditJobComponent {
  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<BackupJob, BackupJobBuilder>,
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly api: ApiService,
    private readonly errorService: ErrorService,
  ) {}

  get job() {
    return this.context.data
  }

  async save() {
    const loader = this.loader.open('Saving Job').subscribe()

    try {
      const { id } = this.job.job
      let job: BackupJob

      if (id) {
        job = await this.api.updateBackupJob(this.job.buildUpdate(id))
      } else {
        job = await this.api.createBackupJob(this.job.buildCreate())
      }

      this.context.completeWith(job)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  presentModalTarget() {
    this.dialogs
      .open<BackupTarget>(new PolymorpheusComponent(TargetSelectPage), {
        label: 'Select Backup Target',
        data: { type: 'create' },
      })
      .subscribe(target => {
        this.job.target = target
      })
  }

  presentModalPackages() {
    this.dialogs
      .open<string[]>(new PolymorpheusComponent(BackupSelectPage), {
        label: 'Select Services to Back Up',
        data: { btnText: 'Done' },
      })
      .subscribe(id => {
        this.job['package-ids'] = id
      })
  }
}
