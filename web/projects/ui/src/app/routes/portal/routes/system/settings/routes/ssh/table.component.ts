import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TuiButtonModule, TuiFadeModule } from '@taiga-ui/experimental'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { filter, take } from 'rxjs'
import { PROMPT } from 'src/app/routes/portal/modals/prompt.component'
import { SSHKey } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'table[keys]',
  template: `
    <thead>
      <tr>
        <th>Hostname</th>
        <th>Created At</th>
        <th>Algorithm</th>
        <th>Fingerprint</th>
        <th></th>
      </tr>
    </thead>
    <tbody>
      @for (key of keys; track $index) {
        <tr>
          <td class="title">{{ key.hostname }}</td>
          <td class="date">{{ key.createdAt | date: 'medium' }}</td>
          <td class="algorithm">{{ key.alg }}</td>
          <td class="fingerprint" tuiFade>{{ key.fingerprint }}</td>
          <td class="actions">
            <button
              tuiIconButton
              size="xs"
              appearance="icon"
              iconLeft="tuiIconTrash2"
              (click)="delete(key)"
            >
              Delete
            </button>
          </td>
        </tr>
      } @empty {
        @if (keys) {
          <tr><td colspan="5">No keys added</td></tr>
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
    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: 2fr 1fr;
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

      .date {
        order: 3;
      }

      .algorithm {
        order: 4;
        text-align: right;
      }

      .fingerprint {
        order: 5;
        grid-column: span 2;
        color: var(--tui-text-02);
      }
    }
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, TuiButtonModule, TuiFadeModule],
})
export class SSHTableComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly cdr = inject(ChangeDetectorRef)

  @Input()
  keys: SSHKey[] | null = null

  add() {
    this.dialogs
      .open<string>(PROMPT, ADD_OPTIONS)
      .pipe(take(1))
      .subscribe(async key => {
        const loader = this.loader.open('Saving...').subscribe()

        try {
          this.keys?.push(await this.api.addSshKey({ key }))
        } finally {
          loader.unsubscribe()
          this.cdr.markForCheck()
        }
      })
  }

  delete(key: SSHKey) {
    this.dialogs
      .open(TUI_PROMPT, DELETE_OPTIONS)
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting...').subscribe()

        try {
          await this.api.deleteSshKey({ fingerprint: key.fingerprint })
          this.keys?.splice(this.keys?.indexOf(key), 1)
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
          this.cdr.markForCheck()
        }
      })
  }
}

const ADD_OPTIONS: Partial<TuiDialogOptions<{ message: string }>> = {
  label: 'SSH Key',
  data: {
    message:
      'Enter the SSH public key you would like to authorize for root access to your Embassy.',
  },
}

const DELETE_OPTIONS: Partial<TuiDialogOptions<TuiPromptData>> = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Delete key? This action cannot be undone.',
    yes: 'Delete',
    no: 'Cancel',
  },
}
