import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  OnInit,
  signal,
} from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { Placeholder } from 'src/app/components/placeholder'
import { ActionService } from 'src/app/services/action.service'
import { ApiService, SshKeyFromApi } from 'src/app/services/api/api.service'
import { ADD_SSH_KEY } from './dialog'

@Component({
  template: `
    <table tuiTable class="g-table" [tuiSkeleton]="loading()">
      <thead>
        <tr>
          <th tuiTh>Hostname</th>
          <th tuiTh>Algorithm</th>
          <th tuiTh colspan="2">
            Fingerprint
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="add()">
              Add SSH key
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (item of keys(); track item.fingerprint) {
          <tr>
            <td tuiTd>{{ item.hostname || '—' }}</td>
            <td tuiTd>{{ item.algorithm }}</td>
            <td tuiTd>{{ item.fingerprint }}</td>
            <td tuiTd>
              <button
                tuiIconButton
                type="button"
                size="xs"
                appearance="icon"
                iconStart="@tui.trash"
                (click)="remove(item)"
              >
                Delete
              </button>
            </td>
          </tr>
        } @empty {
          <tr>
            <td tuiTd colspan="4">
              <app-placeholder icon="@tui.terminal">
                No SSH keys
              </app-placeholder>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    [tuiButton] {
      float: inline-end;
    }

    [tuiTd]:last-child {
      padding-block: 0;
    }

    [tuiTd]:nth-child(3) {
      word-break: break-all;
    }
  `,
  host: { class: 'g-page' },
  imports: [TuiButton, TuiTable, TuiSkeleton, Placeholder],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class SshKeys implements OnInit {
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly actions = inject(ActionService)

  protected readonly keys = signal<SshKeyFromApi[] | null>(null)
  protected readonly loading = computed(() => !this.keys())

  async ngOnInit() {
    this.keys.set(await this.api.sshKeysList())
  }

  protected async remove(key: SshKeyFromApi) {
    if (
      await this.actions.run(
        () => this.api.sshKeysDelete({ fingerprint: key.fingerprint }),
        {
          loading: '',
          success: 'SSH key removed',
          fail: 'Failed to remove SSH key',
        },
      )
    ) {
      this.keys.set(await this.api.sshKeysList())
    }
  }

  protected add() {
    this.dialogs
      .open<string>(ADD_SSH_KEY, { label: 'Add SSH Key' })
      .subscribe(async rawKey => {
        if (
          await this.actions.run(() => this.api.sshKeysAdd({ key: rawKey }), {
            loading: '',
            success: 'SSH key added',
            fail: 'Failed to add SSH key',
          })
        ) {
          this.keys.set(await this.api.sshKeysList())
        }
      })
  }
}
