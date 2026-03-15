import { Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormsModule } from '@angular/forms'
import { ErrorService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiDialogService,
  TuiInput,
} from '@taiga-ui/core'
import {
  TuiBadge,
  TuiNotificationMiddleService,
  TuiSwitch,
} from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { from, map } from 'rxjs'
import { BackupJob } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ToHumanCronPipe } from '../pipes/to-human-cron.pipe'
import { BackupJobBuilder } from '../utils/job-builder'
import { BACKUP, BACKUP_OPTIONS } from './backup.component'
import { TARGET, TARGET_CREATE } from './target.component'

@Component({
  template: `
    <form class="form">
      <tui-textfield>
        <label tuiLabel>Job Name</label>
        <input
          tuiInput
          name="name"
          [(ngModel)]="job.name"
          placeholder="My Backup Job"
        />
      </tui-textfield>
      <button
        tuiButton
        appearance="secondary"
        type="button"
        class="button"
        size="l"
        (click)="selectTarget()"
      >
        Target
        <span
          tuiBadge
          [appearance]="target()?.[job.targetId]?.type ? 'success' : 'warning'"
        >
          {{ target()?.[job.targetId]?.type || 'Select target' }}
        </span>
      </button>
      <button
        tuiButton
        appearance="secondary"
        type="button"
        class="button"
        size="l"
        (click)="selectPackages()"
      >
        Packages
        <span
          tuiBadge
          [appearance]="job.packageIds.length ? 'success' : 'warning'"
        >
          {{ job.packageIds.length + ' selected' }}
        </span>
      </button>
      <tui-textfield>
        <label tuiLabel>Schedule</label>
        <input
          tuiInput
          name="cron"
          [(ngModel)]="job.cron"
          placeholder="* * * * *"
        />
      </tui-textfield>
      @if (job.cron | toHumanCron; as human) {
        <div [style.color]="human.color">{{ human.message }}</div>
      }
      @if (!job.job.id) {
        <div class="g-toggle">
          Also Execute Now
          <input
            tuiSwitch
            type="checkbox"
            name="now"
            [showIcons]="false"
            [(ngModel)]="job.now"
          />
        </div>
      }
      <button
        tuiButton
        class="submit"
        [style.margin-left]="'auto'"
        (click)="save()"
      >
        Save Job
      </button>
    </form>
  `,
  styles: `
    .form {
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }

    .button[data-size] {
      width: unset;
      padding: 1rem;
      text-indent: 0;
      justify-content: space-between;
    }
  `,
  imports: [
    FormsModule,
    TuiInput,
    TuiSwitch,
    TuiButton,
    TuiBadge,
    ToHumanCronPipe,
  ],
})
export class BackupsEditModal {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly context =
    injectContext<TuiDialogContext<BackupJob, BackupJobBuilder>>()

  readonly target = toSignal(
    from(this.api.getBackupTargets({})).pipe(map(({ saved }) => saved)),
  )

  get job() {
    return this.context.data
  }

  async save() {
    const loader = this.loader.open('Saving Job').subscribe()

    try {
      const job = this.job.job.id
        ? await this.api.updateBackupJob(this.job.buildUpdate(this.job.job.id))
        : await this.api.createBackupJob(this.job.buildCreate())

      this.context.completeWith(job)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  selectTarget() {
    this.dialogs
      .open<T.BackupTarget & { id: string }>(TARGET, TARGET_CREATE)
      .subscribe(({ id }) => {
        this.job.targetId = id
      })
  }

  selectPackages() {
    this.dialogs.open<string[]>(BACKUP, BACKUP_OPTIONS).subscribe(id => {
      this.job.packageIds = id
    })
  }
}

export const EDIT = new PolymorpheusComponent(BackupsEditModal)
