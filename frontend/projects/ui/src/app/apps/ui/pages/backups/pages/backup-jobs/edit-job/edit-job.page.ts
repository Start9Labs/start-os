import { Component, Input } from '@angular/core'
import { BackupJob } from 'src/app/services/api/api.types'
import { LoadingController, ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { BackupJobBuilder } from '../job-options/job-options.component'

@Component({
  selector: 'edit-job',
  templateUrl: './edit-job.page.html',
  styleUrls: ['./edit-job.page.scss'],
})
export class EditJobPage {
  @Input() existingJob!: BackupJob

  job = {} as BackupJobBuilder

  saving = false

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
  ) {}

  ngOnInit() {
    this.job = new BackupJobBuilder(this.existingJob)
  }

  async dismiss() {
    this.modalCtrl.dismiss()
  }

  async save() {
    this.saving = true
    const loader = await this.loadingCtrl.create({
      message: 'Saving Job',
    })
    await loader.present()

    try {
      const job = await this.api.updateBackupJob(
        this.job.buildUpdate(this.existingJob.id),
      )
      this.modalCtrl.dismiss(job)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
      this.saving = false
    }
  }
}