import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { from, map } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { CronJob } from 'cron'

@Component({
  selector: 'backups',
  templateUrl: './backups.page.html',
  styleUrls: ['./backups.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BackupsPage {
  readonly secure = this.config.isSecure()
  readonly current$ = this.patch
    .watch$('server-info', 'status-info', 'current-backup', 'job')
    .pipe(map(job => job || {}))
  readonly upcoming$ = from(this.api.getBackupJobs({})).pipe(
    map(jobs =>
      jobs
        .map(job => {
          const nextDate = new CronJob(job.cron, () => {}).nextDate()
          const next = nextDate.toISO()
          const diff = nextDate.diffNow().milliseconds
          return {
            ...job,
            next,
            diff,
          }
        })
        .sort((a, b) => a.diff - b.diff),
    ),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
    private readonly api: ApiService,
  ) {}
}
