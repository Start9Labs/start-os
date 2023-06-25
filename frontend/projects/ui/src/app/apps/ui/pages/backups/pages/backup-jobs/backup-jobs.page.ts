import { Component } from '@angular/core'
import { AlertController, ModalController } from '@ionic/angular'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { BehaviorSubject } from 'rxjs'
import { BackupJob } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { EditJobComponent } from './edit-job/edit-job.component'
import { BackupJobBuilder } from './edit-job/job-builder'

@Component({
  selector: 'backup-jobs',
  templateUrl: './backup-jobs.page.html',
  styleUrls: ['./backup-jobs.page.scss'],
})
export class BackupJobsPage {
  readonly docsUrl =
    'https://docs.start9.com/latest/user-manual/backups/backup-jobs'

  jobs: BackupJob[] = []

  loading$ = new BehaviorSubject(true)

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
  ) {}

  async ngOnInit() {
    try {
      this.jobs = await this.api.getBackupJobs({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading$.next(false)
    }
  }

  presentModalCreate() {
    this.dialogs
      .open<BackupJob>(new PolymorpheusComponent(EditJobComponent), {
        label: 'Create New Job',
        data: new BackupJobBuilder({
          name: `Backup Job ${this.jobs.length + 1}`,
        }),
      })
      .subscribe(job => this.jobs.push(job))
  }

  presentModalUpdate(data: BackupJob) {
    this.dialogs
      .open<BackupJob>(new PolymorpheusComponent(EditJobComponent), {
        label: 'Edit Job',
        data: new BackupJobBuilder(data),
      })
      .subscribe(job => {
        data.name = job.name
        data.target = job.target
        data.cron = job.cron
        data['package-ids'] = job['package-ids']
      })
  }

  async presentAlertDelete(id: string, index: number) {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Delete backup job? This action cannot be undone.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.delete(id, index)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private async delete(id: string, i: number): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.removeBackupTarget({ id })
      this.jobs.splice(i, 1)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
