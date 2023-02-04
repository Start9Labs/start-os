import { Component, Input } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { BackupJobBuilder } from '../job-options/job-options.component'

@Component({
  selector: 'new-job',
  templateUrl: './new-job.page.html',
  styleUrls: ['./new-job.page.scss'],
})
export class NewJobPage {
  @Input() count!: number

  readonly docsUrl =
    'https://docs.start9.com/latest/user-manual/backups/backup-jobs'

  job = new BackupJobBuilder()

  saving = false

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
  ) {}

  async dismiss() {
    this.modalCtrl.dismiss()
  }

  async save() {
    const loader = await this.loadingCtrl.create({
      message: 'Saving Job',
    })
    await loader.present()

    try {
      const job = await this.api.createBackupJob(this.job.buildCreate())
      this.modalCtrl.dismiss(job)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}
