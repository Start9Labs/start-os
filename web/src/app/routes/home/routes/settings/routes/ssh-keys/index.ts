import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  OnInit,
  signal,
} from '@angular/core'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiNotificationService, TuiTitle } from '@taiga-ui/core'
import { TuiNotificationMiddleService, TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { ApiService } from 'src/app/services/api/api.service'
import { Help } from 'src/app/directives/help'

import { SshKeysAside } from './aside'
import { ADD_SSH_KEY } from './dialog'
import { Placeholder } from 'src/app/routes/home/components/placeholder'

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
    <ssh-keys-aside *help />
    <header tuiHeader>
      <h2 tuiTitle>SSH Keys</h2>
      <aside tuiAccessories>
        <button tuiButton iconStart="@tui.plus" (click)="add()">
          Add SSH key
        </button>
      </aside>
    </header>
    <table tuiTable class="g-table" [tuiSkeleton]="loading()">
      <thead>
        <tr>
          <th tuiTh>Hostname</th>
          <th tuiTh>Algorithm</th>
          <th tuiTh>Public Key</th>
          <th tuiTh></th>
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
    [tuiTd]:last-child {
      padding-block: 0;
    }

    [tuiTd]:nth-child(3) {
      word-break: break-all;
    }
  `,
  host: { class: 'g-page' },
  imports: [
    TuiButton,
    TuiTable,
    TuiSkeleton,
    TuiHeader,
    TuiTitle,
    Help,
    SshKeysAside,
    Placeholder,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class SshKeys implements OnInit {
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly alerts = inject(TuiNotificationService)
  private readonly loader = inject(TuiNotificationMiddleService)

  private modified = ''

  protected readonly keys = signal<SshKey[] | null>(null)
  protected readonly loading = computed(() => !this.keys())

  async ngOnInit() {
    const file = await this.api.getFile({ path: AUTHORIZED_KEYS_PATH })
    this.modified = file.modified
    this.keys.set(parseAuthorizedKeys(file.contents))
  }

  protected async remove(index: number) {
    const loading = this.loader.open('').subscribe()
    try {
      const updated = this.keys()?.filter((_, i) => i !== index) ?? []
      await this.api.setFile({
        path: AUTHORIZED_KEYS_PATH,
        contents: updated.map(k => k.raw).join('\n'),
        modified: this.modified,
      })
      this.keys.set(updated)
      this.alerts
        .open('SSH key removed', { appearance: 'positive' })
        .subscribe()
    } catch (e: any) {
      console.error(e)
      this.alerts
        .open(e.message || 'Failed to remove SSH key', {
          appearance: 'negative',
        })
        .subscribe()
    } finally {
      loading.unsubscribe()
    }
  }

  protected add() {
    this.dialogs
      .open<string>(ADD_SSH_KEY, {
        label: 'Add SSH Key',
        size: 'm',
      })
      .subscribe(async rawKey => {
        const loading = this.loader.open('').subscribe()
        try {
          const newKey = parseAuthorizedKey(rawKey)
          const updated = [...(this.keys() ?? []), newKey]
          await this.api.setFile({
            path: AUTHORIZED_KEYS_PATH,
            contents: updated.map(k => k.raw).join('\n'),
            modified: this.modified,
          })
          this.keys.set(updated)
          this.alerts
            .open('SSH key added', { appearance: 'positive' })
            .subscribe()
        } catch (e: any) {
          console.error(e)
          this.alerts
            .open(e.message || 'Failed to add SSH key', {
              appearance: 'negative',
            })
            .subscribe()
        } finally {
          loading.unsubscribe()
        }
      })
  }
}
