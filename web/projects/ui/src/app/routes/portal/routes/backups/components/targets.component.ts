import { KeyValuePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  inject,
  Input,
  Output,
} from '@angular/core'
import {
  TuiDialogOptions,
  TuiDialogService,
  TuiIcon,
  TuiButton,
} from '@taiga-ui/core'
import { TuiConfirmData, TUI_CONFIRM, TuiSkeleton } from '@taiga-ui/kit'
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
      @for (target of backupsTargets || {} | keyvalue; track $index) {
        <tr>
          <td class="title">{{ target.value.name }}</td>
          <td class="type">
            <tui-icon [icon]="target.value.type | getBackupIcon" />
            {{ target.value.type }}
          </td>
          <td class="available">
            <tui-icon
              [icon]="target.value.mountable ? '@tui.check' : '@tui.x'"
              [class]="target.value.mountable ? 'g-positive' : 'g-negative'"
            />
          </td>
          <td class="path">{{ target.value.path }}</td>
          <td class="actions">
            <button
              tuiIconButton
              size="xs"
              appearance="icon"
              iconStart="@tui.pencil"
              (click)="update.emit(target.key)"
            >
              Update
            </button>
            <button
              tuiIconButton
              size="xs"
              appearance="icon"
              iconStart="@tui.trash-2"
              (click)="delete$.next(target.key)"
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
              <td colspan="5"><div [tuiSkeleton]="true">Loading</div></td>
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

      .type {
        order: 1;
        text-transform: capitalize;
        color: var(--tui-text-secondary);
        grid-column: span 3;

        tui-icon {
          display: none;
        }
      }

      .available {
        order: 2;
      }

      .title {
        order: 3;
        font-weight: bold;
        text-transform: uppercase;
      }

      .actions {
        order: 4;
        padding: 0;
        text-align: right;
      }

      .path {
        order: 5;
        color: var(--tui-text-tertiary);
        grid-column: span 2;
        overflow: hidden;
        text-overflow: ellipsis;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButton, GetBackupIconPipe, TuiIcon, KeyValuePipe, TuiSkeleton],
})
export class BackupsTargetsComponent {
  private readonly dialogs = inject(TuiDialogService)

  readonly delete$ = new Subject<string>()

  @Input()
  backupsTargets: Record<string, BackupTarget> | null = null

  @Output()
  readonly update = new EventEmitter<string>()

  @Output()
  readonly delete = this.delete$.pipe(
    switchMap(id =>
      this.dialogs.open(TUI_CONFIRM, OPTIONS).pipe(
        filter(Boolean),
        map(() => id),
      ),
    ),
  )
}

const OPTIONS: Partial<TuiDialogOptions<TuiConfirmData>> = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Forget backup target? This actions cannot be undone.',
    no: 'Cancel',
    yes: 'Delete',
  },
}
