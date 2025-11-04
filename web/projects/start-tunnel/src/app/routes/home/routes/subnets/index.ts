import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { LoadingService } from '@start9labs/shared'
import { utils } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiTextfield,
} from '@taiga-ui/core'
import { TuiDialogService } from '@taiga-ui/experimental'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData } from 'src/app/services/patch-db/data-model'

import { SUBNETS_ADD } from './add'

@Component({
  template: `
    <table class="g-table">
      <thead>
        <tr>
          <th>Name</th>
          <th>IP Range</th>
          <th [style.padding-inline-end.rem]="0.625">
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="onAdd()">
              Add
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (subnet of subnets(); track $index) {
          <tr>
            <td>{{ subnet.name }}</td>
            <td>{{ subnet.range }}</td>
            <td>
              <button
                tuiIconButton
                size="xs"
                tuiDropdown
                tuiDropdownOpen
                appearance="flat-grayscale"
                iconStart="@tui.ellipsis-vertical"
              >
                Actions
                <tui-data-list *tuiTextfieldDropdown size="s">
                  <button
                    tuiOption
                    iconStart="@tui.pencil"
                    new
                    (click)="onEdit(subnet)"
                  >
                    Rename
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.trash"
                    new
                    (click)="onDelete($index)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiDropdown, TuiDataList, TuiTextfield],
})
export default class Subnets {
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)
  private readonly loading = inject(LoadingService)

  protected readonly subnets = toSignal<MappedSubnet[], []>(
    inject<PatchDB<TunnelData>>(PatchDB)
      .watch$('wg', 'subnets')
      .pipe(
        map(s =>
          Object.entries(s).map(([range, info]) => ({
            range,
            name: info.name,
            hasClients: !!Object.keys(info.clients).length,
          })),
        ),
      ),
    { initialValue: [] },
  )

  protected onAdd(): void {
    this.dialogs
      .open(SUBNETS_ADD, {
        label: 'Add Subnet',
        data: { subnet: this.getNext() },
      })
      .subscribe()
  }

  protected onEdit({ range, name }: MappedSubnet): void {
    this.dialogs
      .open(SUBNETS_ADD, {
        label: 'Rename Subnet',
        data: { subnet: range, name },
      })
      .subscribe()
  }

  protected onDelete(index: number): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const subnet = this.subnets()[index]?.range || ''
        const loader = this.loading.open().subscribe()

        try {
          await this.api.deleteSubnet({ subnet })
        } catch (e) {
          console.log(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  private getNext(): string {
    const used = this.subnets().map(s => new utils.IpNet(s.range).octets.at(2))

    for (let i = 0; i < 256; i++) {
      if (!used.includes(i)) {
        return `10.59.${i}.0/24`
      }
    }

    // No recommendation if /24 subnets are used
    return ''
  }
}

type MappedSubnet = {
  range: string
  name: string
  hasClients: boolean
}
