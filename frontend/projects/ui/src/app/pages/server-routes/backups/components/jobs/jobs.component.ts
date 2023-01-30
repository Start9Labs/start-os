import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import {
  BackupJob,
  NewJobComponent,
  NewJobProps,
} from '../new-job/new-job.component'

@Component({
  selector: 'backup-jobs',
  templateUrl: './jobs.component.html',
  styleUrls: ['./jobs.component.scss'],
})
export class BackupJobsComponent {
  readonly docsUrl =
    'https://docs.start9.com/latest/user-manual/backups/backup-jobs'
  jobs: BackupJob[] = []

  constructor(private readonly modalCtrl: ModalController) {}

  async presentModalCreate(job?: BackupJob) {
    const componentProps: NewJobProps = {
      count: this.jobs.length,
    }
    if (job) componentProps.job = job

    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: NewJobComponent,
      componentProps,
    })

    modal.onWillDismiss().then(res => {
      if (res.data) {
        this.jobs.unshift(res.data)
      }
    })

    await modal.present()
  }
}
