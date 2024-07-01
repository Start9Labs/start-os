import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  inject,
  Input,
  Output,
} from '@angular/core'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TuiButtonModule, TuiIconModule } from '@taiga-ui/experimental'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { filter, map, Subject, switchMap } from 'rxjs'
import { BackupTarget } from 'src/app/services/api/api.types'
import { GetBackupIconPipe } from '../pipes/get-backup-icon.pipe'

@Component({
  selector: 'table[backupsTargets]',
  template: `
    <thead>
      <tr>
        <th>Name</th>
        <th>Type</th>
        <th>Available</th>
        <th>Path</th>
        <th [style.width.rem]="3.5"></th>
      </tr>
    </thead>
    <tbody>
      @for (target of backupsTargets; track $index) {
        <tr>
          <td class="title">{{ target.name }}</td>
          <td class="type">
            <tui-icon [icon]="target.type | getBackupIcon" />
            {{ target.type }}
          </td>
          <td class="available">
            <tui-icon
              [icon]="target.mountable ? 'tuiIconCheck' : 'tuiIconClose'"
              [class]="target.mountable ? 'g-success' : 'g-error'"
            />
          </td>
          <td class="path">{{ target.path }}</td>
          <td class="actions">
            <button
              tuiIconButton
              size="xs"
              appearance="icon"
              iconLeft="tuiIconEdit2"
              (click)="update.emit(target)"
            >
              Update
            </button>
            <button
              tuiIconButton
              size="xs"
              appearance="icon"
              iconLeft="tuiIconTrash2"
              (click)="delete$.next(target.id)"
            >
              Delete
            </button>
          </td>
        </tr>
      } @empty {
        @if (backupsTargets) {
          <tr><td colspan="5">No saved backup targets.</td></tr>
        } @else {
          @for (i of ['', '']; track $index) {
            <tr>
              <td colspan="5"><div class="tui-skeleton">Loading</div></td>
            </tr>
          }
        }
      }
    </tbody>
  `,
  styles: `
    tui-icon {
      font-size: 1rem;
      vertical-align: sub;
      margin-inline-end: 0.25rem;
    }

    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: 1.5rem 2fr 1fr;
      }

      td:only-child {
        grid-column: span 3;
      }

      .title {
        order: 2;
        font-weight: bold;
        text-transform: uppercase;
      }

      .available {
        order: 1;
      }

      .actions {
        order: 3;
        padding: 0;
        text-align: right;
      }

      .path {
        order: 4;
        color: var(--tui-text-03);
        grid-column: span 2;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .type {
        order: 5;
        text-align: right;
        text-transform: capitalize;
        color: var(--tui-text-02);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButtonModule, GetBackupIconPipe, TuiIconModule],
})
export class BackupsTargetsComponent {
  private readonly dialogs = inject(TuiDialogService)

  readonly delete$ = new Subject<string>()

  @Input()
  backupsTargets: readonly BackupTarget[] | null = null

  @Output()
  readonly update = new EventEmitter<BackupTarget>()

  @Output()
  readonly delete = this.delete$.pipe(
    switchMap(id =>
      this.dialogs.open(TUI_PROMPT, OPTIONS).pipe(
        filter(Boolean),
        map(() => id),
      ),
    ),
  )
}

const OPTIONS: Partial<TuiDialogOptions<TuiPromptData>> = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Forget backup target? This actions cannot be undone.',
    no: 'Cancel',
    yes: 'Delete',
  },
}
