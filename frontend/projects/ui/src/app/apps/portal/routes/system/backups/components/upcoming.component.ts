import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiForModule } from '@taiga-ui/cdk'
import { TuiSvgModule } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { from, map } from 'rxjs'
import { CronJob } from 'cron'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GetBackupIconPipe } from '../pipes/get-backup-icon.pipe'

@Component({
  selector: 'table[backupsUpcoming]',
  template: `
    <thead>
      <tr>
        <th>Scheduled</th>
        <th>Job</th>
        <th>Target</th>
        <th>Packages</th>
      </tr>
    </thead>
    <tbody *ngIf="current$ | async as current">
      <tr *ngFor="let job of upcoming$ | async; else: loading; empty: blank">
        <td>
          <span
            *ngIf="current.id === job.id; else notRunning"
            [style.color]="'var(--tui-positive)'"
          >
            Running
          </span>
          <ng-template #notRunning>
            {{ job.next | date : 'MMM d, y, h:mm a' }}
          </ng-template>
        </td>
        <td>{{ job.name }}</td>
        <td>
          <tui-svg [src]="job.target.type | getBackupIcon"></tui-svg>
          {{ job.target.name }}
        </td>
        <td>Packages: {{ job['package-ids'].length }}</td>
      </tr>
      <ng-template #blank>
        <tr><td colspan="5">You have no active or upcoming backup jobs</td></tr>
      </ng-template>
      <ng-template #loading>
        <tr *ngFor="let row of ['', '']">
          <td colspan="5"><div class="tui-skeleton">Loading</div></td>
        </tr>
      </ng-template>
    </tbody>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiForModule, TuiSvgModule, GetBackupIconPipe],
})
export class BackupsUpcomingComponent {
  readonly current$ = inject(PatchDB<DataModel>)
    .watch$('server-info', 'status-info', 'current-backup', 'job')
    .pipe(map(job => job || {}))

  readonly upcoming$ = from(inject(ApiService).getBackupJobs({})).pipe(
    map(jobs =>
      jobs
        .map(job => {
          const nextDate = new CronJob(job.cron, () => {}).nextDate()

          return {
            ...job,
            next: nextDate.toISO(),
            diff: nextDate.diffNow().milliseconds,
          }
        })
        .sort((a, b) => a.diff - b.diff),
    ),
  )
}
