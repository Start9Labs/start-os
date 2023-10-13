import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  inject,
  Input,
  Output,
} from '@angular/core'
import { TuiForModule } from '@taiga-ui/cdk'
import {
  TuiButtonModule,
  TuiDialogOptions,
  TuiDialogService,
  TuiSvgModule,
} from '@taiga-ui/core'
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
      <tr *ngFor="let target of backupsTargets; else: loading; empty: blank">
        <td>{{ target.name }}</td>
        <td>
          <tui-svg [src]="target.type | getBackupIcon"></tui-svg>
          {{ target.type | titlecase }}
        </td>
        <td>
          <tui-svg
            [src]="target.mountable ? 'tuiIconCheck' : 'tuiIconClose'"
            [style.color]="
              target.mountable ? 'var(--tui-positive)' : 'var(--tui-negative)'
            "
          ></tui-svg>
        </td>
        <td>{{ target.path }}</td>
        <td>
          <button
            tuiIconButton
            size="xs"
            appearance="icon"
            icon="tuiIconEdit2"
            (click)="update.emit(target)"
          >
            Update
          </button>
          <button
            tuiIconButton
            size="xs"
            appearance="icon"
            icon="tuiIconTrash2"
            (click)="delete$.next(target.id)"
          >
            Delete
          </button>
        </td>
      </tr>
      <ng-template #loading>
        <tr *ngFor="let i of ['', '']">
          <td colspan="5">
            <div class="tui-skeleton">Loading</div>
          </td>
        </tr>
      </ng-template>
      <ng-template #blank>
        <tr><td colspan="5">No saved backup targets.</td></tr>
      </ng-template>
    </tbody>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiForModule,
    TuiSvgModule,
    TuiButtonModule,
    GetBackupIconPipe,
  ],
})
export class BackupsTargetsComponent {
  private readonly dialogs = inject(TuiDialogService)

  readonly delete$ = new Subject<string>()
  readonly update$ = new Subject<BackupTarget>()

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
