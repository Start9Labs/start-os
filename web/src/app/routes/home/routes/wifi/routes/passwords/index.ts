import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import {
  TuiButton,
  TuiDataListComponent,
  TuiDropdownContent,
  TuiDropdownDirective,
  TuiDropdownOpen,
  TuiLink,
  TuiOption,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help'
import { Placeholder } from 'src/app/routes/home/components/placeholder'
import { WifiService } from '../../service'

import { WifiPasswordsAside } from './aside'
import {
  ADD_WIFI_PASSWORD,
  WifiPasswordDialogResult,
  WifiPasswordEntry,
} from './dialog'

@Component({
  template: `
    <wifi-passwords-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Passwords</h2></hgroup>
      <aside tuiAccessories>
        <button tuiButton iconStart="@tui.plus" (click)="add()">Add</button>
      </aside>
    </header>
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh [sorter]="'label' | tuiSorter">Label</th>
          <th tuiTh [sorter]="'password' | tuiSorter">Password</th>
          <th tuiTh [sorter]="'profile' | tuiSorter">Security Profile</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of passwords(); track $index) {
          <tr>
            <td tuiTd>
              <b>{{ item.label }}</b>
            </td>
            <td tuiTd>
              <div class="password-cell">
                <span>
                  {{
                    revealed().has($index) ? item.password : '••••••••••••••••'
                  }}
                </span>
                <button
                  tuiIconButton
                  size="xs"
                  appearance="icon"
                  [iconStart]="
                    revealed().has($index) ? '@tui.eye-off' : '@tui.eye'
                  "
                  (click)="toggleReveal($index)"
                >
                  Toggle visibility
                </button>
                <button
                  tuiIconButton
                  size="xs"
                  appearance="icon"
                  iconStart="@tui.copy"
                  (click)="copyPassword(item.password)"
                >
                  Copy
                </button>
              </div>
            </td>
            <td tuiTd>
              <a tuiLink>{{ item.profile }}</a>
            </td>
            <td tuiTd>
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.ellipsis-vertical"
                appearance="icon"
                tuiDropdownAuto
                tuiDropdown
              >
                Actions
                <tui-data-list
                  *tuiDropdown="let close"
                  size="s"
                  (click)="close()"
                >
                  <button
                    tuiOption
                    iconStart="@tui.pencil"
                    (click)="edit(item, $index)"
                  >
                    Edit
                  </button>
                  <button tuiOption iconStart="@tui.radio">Start WPS</button>
                  <button
                    tuiOption
                    class="g-negative"
                    iconStart="@tui.trash"
                    (click)="delete($index)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        } @empty {
          <tr>
            <td tuiTd colspan="4">
              <app-placeholder icon="@tui.wifi">
                No Wi-Fi passwords configured
              </app-placeholder>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      max-width: 50rem;
    }

    td:last-child {
      text-align: end;
    }

    .password-cell {
      display: flex;
      align-items: center;
      gap: 0.25rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    TuiHeader,
    TuiTitle,
    TuiButton,
    TuiTable,
    TuiLink,
    TuiDataListComponent,
    TuiDropdownContent,
    TuiDropdownDirective,
    TuiDropdownOpen,
    TuiOption,
    Help,
    Placeholder,
    WifiPasswordsAside,
  ],
})
export default class WifiPasswords {
  private readonly service = inject(WifiService)
  private readonly dialogs = inject(TuiResponsiveDialogService)

  protected readonly revealed = signal(new Set<number>())

  protected readonly passwords = computed(() => {
    const config = this.service.data()
    if (!config) return []
    return config.passwords.map(p => ({
      label: p.label,
      password: p.password,
      profile: p.profile?.fullname ?? 'Admin',
    }))
  })

  add() {
    this.dialogs
      .open<WifiPasswordDialogResult>(ADD_WIFI_PASSWORD, {
        label: 'Add Wi-Fi Password',
        data: {
          profiles: ['Admin', 'Guest'],
        },
      })
      .subscribe(result => {
        this.service.addPassword({
          label: result.label,
          profile:
            result.profile === 'Admin'
              ? null
              : {
                  fullname: result.profile,
                  interface: result.profile.toLowerCase(),
                  vlan_tag: 100,
                },
          password: result.password!,
        })
      })
  }

  edit(entry: WifiPasswordEntry, index: number) {
    this.dialogs
      .open<WifiPasswordDialogResult>(ADD_WIFI_PASSWORD, {
        label: 'Edit Wi-Fi Password',
        data: {
          profiles: ['Admin', 'Guest'],
          entry,
        },
      })
      .subscribe(result => {
        const current = this.service.data()
        if (!current) return
        const passwords = [...current.passwords]
        passwords[index] = {
          ...passwords[index],
          label: result.label,
          profile:
            result.profile === 'Admin'
              ? null
              : {
                  fullname: result.profile,
                  interface: result.profile.toLowerCase(),
                  vlan_tag: 100,
                },
        }
        this.service
          .store({ ...current, passwords })
          .then(() => this.service.refresh())
      })
  }

  toggleReveal(index: number) {
    const next = new Set(this.revealed())
    if (next.has(index)) {
      next.delete(index)
    } else {
      next.add(index)
    }
    this.revealed.set(next)
  }

  copyPassword(password: string) {
    navigator.clipboard.writeText(password)
  }

  delete(index: number) {
    this.service.deletePassword(index)
  }
}
