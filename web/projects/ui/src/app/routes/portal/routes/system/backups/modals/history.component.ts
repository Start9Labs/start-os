import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import {
  ALWAYS_FALSE_HANDLER,
  ALWAYS_TRUE_HANDLER,
  TuiForModule,
} from '@taiga-ui/cdk'
import { TuiDialogService, TuiLinkModule, TuiSvgModule } from '@taiga-ui/core'
import { TuiButtonModule, TuiFadeModule } from '@taiga-ui/experimental'
import { TuiCheckboxModule } from '@taiga-ui/kit'
import { BehaviorSubject } from 'rxjs'
import { BackupRun } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { REPORT } from 'src/app/components/report.component'
import { DurationPipe } from '../pipes/duration.pipe'
import { HasErrorPipe } from '../pipes/has-error.pipe'
import { GetBackupIconPipe } from '../pipes/get-backup-icon.pipe'

@Component({
  template: `
    <ng-container *ngIf="loading$ | async"></ng-container>
    <h3 class="g-title">
      Past Events
      <button
        tuiButton
        appearance="danger-solid"
        [disabled]="disabled"
        (click)="delete()"
      >
        Delete Selected
      </button>
    </h3>
    <div tuiFade class="g-hidden-scrollbar">
      <table class="g-table">
        <thead>
          <tr>
            <th>
              <tui-checkbox
                [disabled]="!selected.length"
                [ngModel]="all"
                (ngModelChange)="toggle()"
              ></tui-checkbox>
            </th>
            <th>Started At</th>
            <th>Duration</th>
            <th>Result</th>
            <th>Job</th>
            <th>Target</th>
          </tr>
        </thead>
        <tbody>
          <tr
            *ngFor="
              let run of runs;
              let index = index;
              else: loading;
              empty: blank
            "
            [style.background]="selected[index] ? 'var(--tui-clear)' : ''"
          >
            <td><tui-checkbox [(ngModel)]="selected[index]"></tui-checkbox></td>
            <td>{{ run.startedAt | date: 'medium' }}</td>
            <td>{{ run.startedAt | duration: run.completedAt }} Minutes</td>
            <td>
              <tui-svg
                *ngIf="run.report | hasError; else noError"
                src="tuiIconClose"
                [style.color]="'var(--tui-negative)'"
              ></tui-svg>
              <ng-template #noError>
                <tui-svg
                  src="tuiIconCheck"
                  [style.color]="'var(--tui-positive)'"
                ></tui-svg>
              </ng-template>
              <button tuiLink (click)="showReport(run)">Report</button>
            </td>
            <td>{{ run.job.name || 'No job' }}</td>
            <td>
              <tui-svg [src]="run.job.target.type | getBackupIcon"></tui-svg>
              {{ run.job.target.name }}
            </td>
          </tr>
          <ng-template #loading>
            <tr *ngFor="let row of ['', '', '']">
              <td colspan="6"><div class="tui-skeleton">Loading</div></td>
            </tr>
          </ng-template>
          <ng-template #blank>
            <tr><td colspan="6">No backups have been run yet.</td></tr>
          </ng-template>
        </tbody>
      </table>
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiForModule,
    TuiButtonModule,
    TuiCheckboxModule,
    TuiSvgModule,
    TuiLinkModule,
    TuiFadeModule,
    DurationPipe,
    HasErrorPipe,
    GetBackupIconPipe,
  ],
})
export class BackupsHistoryModal {
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)

  readonly loading$ = new BehaviorSubject(true)

  runs: BackupRun[] | null = null
  selected: boolean[] = []

  get all(): boolean | null {
    if (this.selected.length === 0) return false

    const response = this.selected[0]

    for (let i = 1; i < this.selected.length; i++) {
      if (this.selected[i] !== response) {
        return null
      }
    }

    return response
  }

  get disabled() {
    return !this.selected.length || !this.selected.some(Boolean)
  }

  async ngOnInit() {
    try {
      this.runs = await this.api.getBackupRuns({})
      this.selected = this.runs.map(ALWAYS_FALSE_HANDLER)
    } catch (e: any) {
      this.runs = []
      this.errorService.handleError(e)
    } finally {
      this.loading$.next(false)
    }
  }

  async delete() {
    const loader = this.loader.open('Deleting...').subscribe()
    const ids = this.selected
      .filter(Boolean)
      .map((_, i) => this.runs?.[i].id || '')

    try {
      await this.api.deleteBackupRuns({ ids })
      this.runs = this.runs?.filter(r => !ids.includes(r.id)) || []
      this.selected = this.runs.map(ALWAYS_FALSE_HANDLER)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  showReport(run: BackupRun) {
    this.dialogs
      .open(REPORT, {
        label: 'Backup Report',
        data: {
          report: run.report,
          timestamp: run.completedAt,
        },
      })
      .subscribe()
  }

  toggle() {
    if (this.all) {
      this.selected = this.selected.map(ALWAYS_FALSE_HANDLER)
    } else {
      this.selected = this.selected.map(ALWAYS_TRUE_HANDLER)
    }
  }
}

export const HISTORY = new PolymorpheusComponent(BackupsHistoryModal)
