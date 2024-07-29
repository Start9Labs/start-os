import { TuiCheckbox } from '@taiga-ui/kit'
import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TUI_TRUE_HANDLER, TUI_FALSE_HANDLER } from '@taiga-ui/cdk'
import { TuiDialogService, TuiIcon, TuiLink, TuiButton } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { REPORT } from 'src/app/components/report.component'
import { BackupRun } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DurationPipe } from '../pipes/duration.pipe'
import { GetBackupIconPipe } from '../pipes/get-backup-icon.pipe'
import { HasErrorPipe } from '../pipes/has-error.pipe'

@Component({
  template: `
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
    <table class="g-table">
      <thead>
        <tr>
          <th>
            <input
              type="checkbox"
              size="s"
              tuiCheckbox
              [disabled]="!selected.length"
              [ngModel]="all"
              (ngModelChange)="toggle()"
            />
          </th>
          <th>Started At</th>
          <th>Duration</th>
          <th>Job</th>
          <th>Result</th>
          <th>Target</th>
        </tr>
      </thead>
      <tbody>
        @for (run of runs(); track $index) {
          <tr>
            <td class="checkbox">
              <input
                type="checkbox"
                tuiCheckbox
                size="s"
                [(ngModel)]="selected[$index]"
              />
            </td>
            <td class="date">{{ run.startedAt | date: 'medium' }}</td>
            <td class="duration">
              {{ run.startedAt | duration: run.completedAt }} minutes
            </td>
            <td class="title">{{ run.job.name || 'No job' }}</td>
            <td class="result">
              @if (run.report | hasError) {
                <tui-icon icon="@tui.x" class="g-error" />
              } @else {
                <tui-icon icon="@tui.check" class="g-success" />
              }
              <button tuiLink (click)="showReport(run)">Report</button>
            </td>
            <td [style.grid-column]="'span 2'">
              <tui-icon [icon]="run.job.target.type | getBackupIcon" />
              {{ run.job.target.name }}
            </td>
          </tr>
        } @empty {
          @if (runs()) {
            <tr><td colspan="6">No backups have been run yet.</td></tr>
          } @else {
            @for (row of ['', '']; track $index) {
              <tr>
                <td colspan="6"><div class="tui-skeleton">Loading</div></td>
              </tr>
            }
          }
        }
      </tbody>
    </table>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    tui-icon {
      font-size: 1rem;
      vertical-align: sub;
      margin-inline-end: 0.25rem;
    }

    button {
      position: relative;
    }

    [tuiCheckbox] {
      display: block;
    }

    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: 1fr 7rem;
      }

      td:only-child {
        grid-column: span 2;
      }

      .checkbox {
        @include fullsize();

        [tuiCheckbox] {
          @include fullsize();
          opacity: 0;
        }
      }

      .title {
        font-weight: bold;
        text-transform: uppercase;
      }

      .date,
      .duration {
        order: 1;
        color: var(--tui-text-secondary);
      }

      .duration,
      .result {
        text-align: right;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiButton,
    TuiIcon,
    TuiLink,
    DurationPipe,
    HasErrorPipe,
    GetBackupIconPipe,
    TuiCheckbox,
  ],
})
export class BackupsHistoryModal {
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)

  runs = signal<BackupRun[] | null>(null)
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
      this.runs.set(await this.api.getBackupRuns({}))
      this.selected = this.runs()?.map(TUI_FALSE_HANDLER) || []
    } catch (e: any) {
      this.runs.set([])
      this.errorService.handleError(e)
    }
  }

  async delete() {
    const loader = this.loader.open('Deleting...').subscribe()
    const ids = this.selected
      .filter(Boolean)
      .map((_, i) => this.runs()?.[i].id || '')

    try {
      await this.api.deleteBackupRuns({ ids })
      this.runs.set(this.runs()?.filter(r => !ids.includes(r.id)) || [])
      this.selected = this.runs()?.map(TUI_FALSE_HANDLER) || []
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
      this.selected = this.selected.map(TUI_FALSE_HANDLER)
    } else {
      this.selected = this.selected.map(TUI_TRUE_HANDLER)
    }
  }
}

export const HISTORY = new PolymorpheusComponent(BackupsHistoryModal)