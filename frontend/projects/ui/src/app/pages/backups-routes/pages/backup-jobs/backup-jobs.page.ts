import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { BehaviorSubject, Subject } from 'rxjs'
import { BackupJob } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { EditJobPage } from './edit-job/edit-job.page'
import { NewJobPage } from './new-job/new-job.page'

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
  error$ = new Subject<string>()

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly api: ApiService,
  ) {}

  async ngOnInit() {
    try {
      this.jobs = await this.api.getBackupJobs({})
    } catch (e: any) {
      this.error$.next(e.message)
    } finally {
      this.loading$.next(false)
    }
  }

  async presentModalCreate() {
    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: NewJobPage,
      componentProps: {
        count: this.jobs.length + 1,
      },
    })

    modal.onWillDismiss().then(res => {
      if (res.data) {
        this.jobs.push(res.data)
      }
    })

    await modal.present()
  }

  async presentModalUpdate(job: BackupJob) {
    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: EditJobPage,
      componentProps: {
        existingJob: job,
      },
    })

    modal.onWillDismiss().then((res: { data?: BackupJob }) => {
      if (res.data) {
        const { name, target, cron } = res.data
        job.name = name
        job.target = target
        job.cron = cron
        job['package-ids'] = res.data['package-ids']
      }
    })

    await modal.present()
  }
}
