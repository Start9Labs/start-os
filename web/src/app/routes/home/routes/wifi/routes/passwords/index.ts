import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiDataList, TuiDropdown, TuiLink } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { Masked } from 'src/app/components/masked'
import { Placeholder } from 'src/app/components/placeholder'
import { Help } from 'src/app/directives/help'
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
    <table tuiTable class="g-table" [tuiSkeleton]="!service.data()">
      <thead>
        <tr>
          <th tuiTh [sorter]="'label' | tuiSorter">Label</th>
          <th tuiTh [sorter]="'password' | tuiSorter">Password</th>
          <th tuiTh [sorter]="'profile' | tuiSorter">Security Profile</th>
          <th tuiTh>
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="add()">
              Add
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (item of passwords() | tuiTableSort; track $index) {
          <tr>
            <td tuiTd>{{ item.label }}</td>
            <td tuiTd>
              <span [appMasked]="item.password"></span>
            </td>
            <td tuiTd>
              <a tuiLink routerLink="/profiles">{{ item.profile }}</a>
            </td>
            <td tuiTd>
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.ellipsis-vertical"
                appearance="icon"
                tuiDropdownAlign="end"
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

    td:first-child {
      font-weight: bold;
    }

    th:last-child,
    td:last-child {
      text-align: end;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    RouterLink,
    TuiButton,
    TuiTable,
    TuiLink,
    TuiDataList,
    TuiDropdown,
    TuiSkeleton,
    Masked,
    Help,
    Placeholder,
    WifiPasswordsAside,
  ],
})
export default class WifiPasswords {
  private readonly dialogs = inject(TuiResponsiveDialogService)

  protected readonly service = inject(WifiService)
  protected readonly passwords = computed(
    () =>
      this.service.data()?.passwords.map(p => ({
        ...p,
        profile: p.profile?.fullname ?? 'Admin',
      })) || [],
  )

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

  delete(index: number) {
    this.service.deletePassword(index)
  }
}
