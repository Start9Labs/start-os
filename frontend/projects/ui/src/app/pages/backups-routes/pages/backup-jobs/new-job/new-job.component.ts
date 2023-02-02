import { Component, Input } from '@angular/core'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ModalController } from '@ionic/angular'
import { BackupSelectPage } from 'src/app/pages/backups-routes/modals/backup-select/backup-select.page'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TargetSelectPage } from '../../../modals/target-select/target-select.page'
import { WithId } from '../../backup-targets/backup-targets.page'

export interface NewJobProps {
  count: number
  job?: BackupJob
}

@Component({
  selector: 'new-job',
  templateUrl: './new-job.component.html',
  styleUrls: ['./new-job.component.scss'],
})
export class NewJobComponent {
  @Input() count!: number
  @Input() job = new BackupJob()
  readonly docsUrl =
    'https://docs.start9.com/latest/user-manual/backups/backup-jobs'
  saving = false

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly api: ApiService,
  ) {}

  ngOnInit() {
    this.job.name = `Job ${this.count + 1}`
  }

  async dismiss() {
    this.modalCtrl.dismiss()
  }

  async presentModalTarget() {
    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: TargetSelectPage,
    })

    modal.onWillDismiss<WithId<BackupTarget>>().then(res => {
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
      componentProps: {
        btnText: 'Done',
      },
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
  name = ''
  target?: WithId<BackupTarget>
  cron = '0 2 * * *' // '* * * * * *' https://cloud.google.com/scheduler/docs/configuring/cron-job-schedules
  'package-ids' = []
  now = false
}
