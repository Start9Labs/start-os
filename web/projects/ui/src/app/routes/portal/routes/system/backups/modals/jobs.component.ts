import { Component, inject, OnInit } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import {
  TuiDialogOptions,
  TuiDialogService,
  TuiNotificationModule,
} from '@taiga-ui/core'
import { TuiButtonModule, TuiIconModule } from '@taiga-ui/experimental'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { BehaviorSubject, filter } from 'rxjs'
import { BackupJob } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GetBackupIconPipe } from '../pipes/get-backup-icon.pipe'
import { ToHumanCronPipe } from '../pipes/to-human-cron.pipe'
import { BackupJobBuilder } from '../utils/job-builder'
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
        @for (job of jobs; track $index) {
          <tr>
            <td class="title">{{ job.name }}</td>
            <td class="target">
              <tui-icon [icon]="job.target.type | getBackupIcon" />
              {{ job.target.name }}
            </td>
            <td class="packages">Packages: {{ job.packageIds.length }}</td>
            <td class="schedule">{{ (job.cron | toHumanCron).message }}</td>
            <td class="actions">
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
        } @empty {
          @if (jobs) {
            <tr><td colspan="5">No jobs found.</td></tr>
          } @else {
            @for (i of ['', '']; track $index) {
              <tr>
                <td colspan="5"><div class="tui-skeleton">Loading</div></td>
              </tr>
            }
          }
        }
      </tbody>
    </table>
  `,
  styles: `
    tui-icon {
      font-size: 1rem;
      vertical-align: sub;
      margin-inline-end: 0.25rem;
    }

    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: 1fr 1fr;
      }

      td:only-child {
        grid-column: span 2;
      }

      .title {
        order: 1;
        font-weight: bold;
        text-transform: uppercase;
      }

      .actions {
        order: 2;
        padding: 0;
        text-align: right;
      }

      .target {
        order: 3;
      }

      .packages {
        order: 4;
        text-align: right;
      }

      .schedule {
        order: 5;
        color: var(--tui-text-02);
      }
    }
  `,
  standalone: true,
  imports: [
    TuiNotificationModule,
    TuiButtonModule,
    TuiIconModule,
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
