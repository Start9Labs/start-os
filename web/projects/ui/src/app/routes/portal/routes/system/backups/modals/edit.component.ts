import { CommonModule } from '@angular/common'
import { Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { ErrorService, LoadingService } from '@start9labs/shared'
import {
  TuiDialogContext,
  TuiDialogService,
  TuiWrapperModule,
} from '@taiga-ui/core'
import { TuiBadgeModule, TuiButtonModule } from '@taiga-ui/experimental'
import {
  TuiInputModule,
  TuiInputNumberModule,
  TuiToggleModule,
} from '@taiga-ui/kit'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupJob, BackupTarget } from 'src/app/services/api/api.types'
import { TARGET, TARGET_CREATE } from './target.component'
import { BACKUP, BACKUP_OPTIONS } from './backup.component'
import { BackupJobBuilder } from '../utils/job-builder'
import { ToHumanCronPipe } from '../pipes/to-human-cron.pipe'

@Component({
  template: `
    <form class="form">
      <tui-input name="name" [(ngModel)]="job.name">
        Job Name
        <input tuiTextfield placeholder="My Backup Job" />
      </tui-input>
      <button
        tuiButton
        appearance="secondary"
        type="button"
        class="button"
        size="l"
        (click)="selectTarget()"
      >
        Target
        <tui-badge [appearance]="job.target.type ? 'success' : 'warning'">
          {{ job.target.type || 'Select target' }}
        </tui-badge>
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
        <tui-badge [appearance]="job.packageIds.length ? 'success' : 'warning'">
          {{ job.packageIds.length + ' selected' }}
        </tui-badge>
      </button>
      <tui-input name="cron" [(ngModel)]="job.cron">
        Schedule
        <input tuiTextfield placeholder="* * * * *" />
      </tui-input>
      <div *ngIf="job.cron | toHumanCron as human" [style.color]="human.color">
        {{ human.message }}
      </div>
      <div *ngIf="!job.job.id" class="g-toggle">
        Also Execute Now
        <tui-toggle size="l" name="now" [(ngModel)]="job.now"></tui-toggle>
      </div>
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
  styles: [
    `
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
  ],
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiInputModule,
    TuiInputNumberModule,
    TuiToggleModule,
    TuiWrapperModule,
    TuiButtonModule,
    TuiBadgeModule,
    ToHumanCronPipe,
  ],
})
export class BackupsEditModal {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly context =
    inject<TuiDialogContext<BackupJob, BackupJobBuilder>>(POLYMORPHEUS_CONTEXT)

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
    this.dialogs.open<BackupTarget>(TARGET, TARGET_CREATE).subscribe(target => {
      this.job.target = target
    })
  }

  selectPackages() {
    this.dialogs.open<string[]>(BACKUP, BACKUP_OPTIONS).subscribe(id => {
      this.job.packageIds = id
    })
  }
}

export const EDIT = new PolymorpheusComponent(BackupsEditModal)
