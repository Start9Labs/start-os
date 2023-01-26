import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { BackupJob, BackupJobModal } from './backup-job-modal.page'

@Component({
  selector: 'backup-jobs',
  templateUrl: './backup-jobs.page.html',
  styleUrls: ['./backup-jobs.page.scss'],
})
export class BackupJobsPage {
  readonly docsUrl =
    'https://docs.start9.com/latest/user-manual/backups/backup-jobs'
  jobs: BackupJob[] = []

  constructor(private readonly modalCtrl: ModalController) {}

  async presentModalCreate() {
    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: BackupJobModal,
    })

    modal.onWillDismiss().then(res => {
      if (res.data) {
        this.jobs.unshift(res.data.job)
      }
    })

    await modal.present()
  }
}
