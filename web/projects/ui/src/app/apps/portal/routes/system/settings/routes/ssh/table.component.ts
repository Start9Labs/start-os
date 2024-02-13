import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  inject,
  Input,
} from '@angular/core'
import {
  TuiDialogOptions,
  TuiDialogService,
  TuiLinkModule,
} from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { PROMPT } from 'src/app/apps/portal/modals/prompt.component'
import { SSHKey } from 'src/app/services/api/api.types'
import { filter, take } from 'rxjs'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { TuiForModule } from '@taiga-ui/cdk'

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
      <tr *ngFor="let key of keys; else: loading">
        <td>{{ key.hostname }}</td>
        <td>{{ key['created-at'] | date: 'medium' }}</td>
        <td>{{ key.alg }}</td>
        <td>{{ key.fingerprint }}</td>
        <td>
          <button
            tuiIconButton
            size="xs"
            appearance="icon"
            iconLeft="tuiIconTrash2"
            [style.display]="'flex'"
            (click)="delete(key)"
          >
            Delete
          </button>
        </td>
      </tr>
      <ng-template #loading>
        <tr *ngFor="let _ of ['', '']">
          <td colspan="5"><div class="tui-skeleton">Loading</div></td>
        </tr>
      </ng-template>
    </tbody>
  `,
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, TuiForModule, TuiButtonModule, TuiLinkModule],
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
