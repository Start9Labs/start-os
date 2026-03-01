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
import { ApiService } from 'src/app/services/api/api.service'
import { ADD_SSH_KEY } from './dialog'

const AUTHORIZED_KEYS_PATH = '/root/.ssh/authorized_keys'

interface SshKey {
  raw: string
  algorithm: string
  publicKey: string
  hostname: string
}

function parseAuthorizedKey(line: string): SshKey {
  const trimmed = line.trim()
  const parts = trimmed.split(/\s+/)
  const algorithm = parts[0] || ''
  const publicKey = parts[1] || ''
  const hostname = parts.slice(2).join(' ') || ''
  return { raw: trimmed, algorithm, publicKey, hostname }
}

function parseAuthorizedKeys(contents: string): SshKey[] {
  return contents
    .split('\n')
    .map(line => line.trim())
    .filter(line => line && !line.startsWith('#'))
    .map(parseAuthorizedKey)
}

@Component({
  template: `
    <table tuiTable class="g-table" [tuiSkeleton]="loading()">
      <thead>
        <tr>
          <th tuiTh>Hostname</th>
          <th tuiTh>Algorithm</th>
          <th tuiTh colspan="2">
            Public Key
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="add()">
              Add SSH key
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (item of keys(); track item.raw; let i = $index) {
          <tr>
            <td tuiTd>{{ item.hostname || '—' }}</td>
            <td tuiTd>{{ item.algorithm }}</td>
            <td tuiTd>{{ item.publicKey }}</td>
            <td tuiTd>
              <button
                tuiIconButton
                type="button"
                size="xs"
                appearance="icon"
                iconStart="@tui.trash"
                (click)="remove(i)"
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

  private modified = ''

  protected readonly keys = signal<SshKey[] | null>(null)
  protected readonly loading = computed(() => !this.keys())

  async ngOnInit() {
    const file = await this.api.getFile({ path: AUTHORIZED_KEYS_PATH })
    this.modified = file.modified
    this.keys.set(parseAuthorizedKeys(file.contents))
  }

  protected async remove(index: number) {
    const updated = this.keys()?.filter((_, i) => i !== index) ?? []

    if (
      await this.actions.run(
        () =>
          this.api.setFile({
            path: AUTHORIZED_KEYS_PATH,
            contents: updated.map(k => k.raw).join('\n'),
            modified: this.modified,
          }),
        {
          loading: '',
          success: 'SSH key removed',
          fail: 'Failed to remove SSH key',
        },
      )
    ) {
      this.keys.set(updated)
    }
  }

  protected add() {
    this.dialogs
      .open<string>(ADD_SSH_KEY, { label: 'Add SSH Key' })
      .subscribe(async rawKey => {
        const newKey = parseAuthorizedKey(rawKey)
        const updated = [...(this.keys() ?? []), newKey]

        if (
          await this.actions.run(
            () =>
              this.api.setFile({
                path: AUTHORIZED_KEYS_PATH,
                contents: updated.map(k => k.raw).join('\n'),
                modified: this.modified,
              }),
            { success: 'SSH key added', fail: 'Failed to add SSH key' },
          )
        ) {
          this.keys.set(updated)
        }
      })
  }
}
