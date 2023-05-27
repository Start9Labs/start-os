import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { BackupJob, BackupTarget, RR } from 'src/app/services/api/api.types'
import { BackupSelectPage } from '../../../modals/backup-select/backup-select.page'
import { TargetSelectPage } from '../../../modals/target-select/target-select.page'

@Component({
  selector: 'job-options',
  templateUrl: './job-options.component.html',
  styleUrls: ['./job-options.component.scss'],
})
export class JobOptionsComponent {
  @Input() job!: BackupJobBuilder

  constructor(private readonly modalCtrl: ModalController) {}

  async presentModalTarget() {
    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: TargetSelectPage,
      componentProps: { type: 'create' },
    })

    modal.onWillDismiss<BackupTarget>().then(res => {
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
        selectedIds: this.job['package-ids'],
      },
    })

    modal.onWillDismiss().then(res => {
      if (res.data) {
        this.job['package-ids'] = res.data
      }
    })

    await modal.present()
  }
}

export class BackupJobBuilder {
  name: string
  target: BackupTarget
  cron: string
  'package-ids': string[]
  now = false

  constructor(readonly job: Partial<BackupJob>) {
    const { name, target, cron } = job
    this.name = name || ''
    this.target = target || ({} as BackupTarget)
    this.cron = cron || '0 2 * * *'
    this['package-ids'] = job['package-ids'] || []
  }

  buildCreate(): RR.CreateBackupJobReq {
    const { name, target, cron, now } = this

    return {
      name,
      'target-id': target.id,
      cron,
      'package-ids': this['package-ids'],
      now,
    }
  }

  buildUpdate(id: string): RR.UpdateBackupJobReq {
    const { name, target, cron } = this

    return {
      id,
      name,
      'target-id': target.id,
      cron,
      'package-ids': this['package-ids'],
    }
  }
}
