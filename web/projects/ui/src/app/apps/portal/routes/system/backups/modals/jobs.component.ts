import { CommonModule } from '@angular/common'
import { Component, inject, OnInit } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiForModule } from '@taiga-ui/cdk'
import {
  TuiDialogOptions,
  TuiDialogService,
  TuiNotificationModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { TuiButtonModule, TuiFadeModule } from '@taiga-ui/experimental'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { BehaviorSubject, filter } from 'rxjs'
import { BackupJob } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupJobBuilder } from '../utils/job-builder'
import { ToHumanCronPipe } from '../pipes/to-human-cron.pipe'
import { GetBackupIconPipe } from '../pipes/get-backup-icon.pipe'
import { EDIT } from './edit.component'

@Component({
  template: `
    <tui-notification>
      Scheduling automatic backups is an excellent way to ensure your StartOS
      data is safely backed up. StartOS will issue a notification whenever one
      of your scheduled backups succeeds or fails.
      <a
        href="https://docs.start9.com/latest/user-manual/backups/backup-jobs"
        target="_blank"
        rel="noreferrer"
      >
        View instructions
      </a>
    </tui-notification>
    <h3 class="g-title">
      Saved Jobs
      <button tuiButton size="s" iconLeft="tuiIconPlus" (click)="create()">
        Create New Job
      </button>
    </h3>
    <div class="g-hidden-scrollbar" tuiFade>
      <table class="g-table">
        <thead>
          <tr>
            <th>Name</th>
            <th>Target</th>
            <th>Packages</th>
            <th>Schedule</th>
            <th [style.width.rem]="3.5"></th>
          </tr>
        </thead>
        <tbody>
          <tr *ngFor="let job of jobs || null; else: loading; empty: blank">
            <td>{{ job.name }}</td>
            <td>
              <tui-svg [src]="job.target.type | getBackupIcon"></tui-svg>
              {{ job.target.name }}
            </td>
            <td>Packages: {{ job.packageIds.length }}</td>
            <td>{{ (job.cron | toHumanCron).message }}</td>
            <td>
              <button
                tuiIconButton
                appearance="icon"
                size="xs"
                iconLeft="tuiIconEdit2"
                (click)="update(job)"
              ></button>
              <button
                tuiIconButton
                appearance="icon"
                size="xs"
                iconLeft="tuiIconTrash2"
                (click)="delete(job.id)"
              ></button>
            </td>
          </tr>
          <ng-template #loading>
            <tr *ngFor="let i of ['', '']">
              <td colspan="5"><div class="tui-skeleton">Loading</div></td>
            </tr>
          </ng-template>
          <ng-template #blank>
            <tr><td colspan="5">No jobs found.</td></tr>
          </ng-template>
        </tbody>
      </table>
    </div>
  `,
  standalone: true,
  imports: [
    CommonModule,
    TuiForModule,
    TuiNotificationModule,
    TuiButtonModule,
    TuiSvgModule,
    TuiFadeModule,
    ToHumanCronPipe,
    GetBackupIconPipe,
  ],
})
export class BackupsJobsModal implements OnInit {
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  readonly loading$ = new BehaviorSubject(true)

  jobs?: BackupJob[]

  async ngOnInit() {
    try {
      this.jobs = await this.api.getBackupJobs({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading$.next(false)
    }
  }

  create() {
    this.dialogs
      .open<BackupJob>(EDIT, {
        label: 'Create New Job',
        data: new BackupJobBuilder({
          name: `Backup Job ${(this.jobs?.length || 0) + 1}`,
        }),
      })
      .subscribe(job => {
        this.jobs = this.jobs?.concat(job)
      })
  }

  update(data: BackupJob) {
    this.dialogs
      .open<BackupJob>(EDIT, {
        label: 'Edit Job',
        data: new BackupJobBuilder(data),
      })
      .subscribe(job => {
        data.name = job.name
        data.target = job.target
        data.cron = job.cron
        data.packageIds = job.packageIds
      })
  }

  delete(id: string) {
    this.dialogs
      .open(TUI_PROMPT, PROMPT_OPTIONS)
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting...').subscribe()

        try {
          await this.api.removeBackupTarget({ id })
          this.jobs = this.jobs?.filter(a => a.id !== id)
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}

const PROMPT_OPTIONS: Partial<TuiDialogOptions<TuiPromptData>> = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Delete backup job? This action cannot be undone.',
    yes: 'Delete',
    no: 'Cancel',
  },
}

export const JOBS = new PolymorpheusComponent(BackupsJobsModal)
