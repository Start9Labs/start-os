import { Component } from '@angular/core'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ModalController } from '@ionic/angular'
import { BackupSelectPage } from 'src/app/modals/backup-select/backup-select.page'
import { BackupDrivesComponent } from '../backup-drives/backup-drives.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'backup-job-modal',
  templateUrl: './backup-job-modal.page.html',
  styleUrls: ['./backup-jobs.page.scss'],
})
export class BackupJobModal {
  readonly docsUrl =
    'https://docs.start9.com/latest/user-manual/backups/backup-jobs'
  saving = false

  job = new BackupJob()

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly api: ApiService,
  ) {}

  async dismiss() {
    this.modalCtrl.dismiss()
  }

  async presentModalTarget() {
    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: BackupDrivesComponent,
    })

    modal.onWillDismiss().then(res => {
      if (res.data) {
        this.job.target = res.data
      }
    })

    await modal.present()
  }

  async presentModalPackages() {
    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: BackupSelectPage,
    })

    modal.onWillDismiss().then(res => {
      if (res.data) {
        this.job['package-ids'] = res.data
      }
    })

    await modal.present()
  }

  async save() {
    this.modalCtrl.dismiss(this.job)
  }
}

export class BackupJob {
  target: BackupTarget | null = null
  cron = '0 2 * * *' // '* * * * * *' https://cloud.google.com/scheduler/docs/configuring/cron-job-schedules
  'package-ids' = []
}
