import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  Signal,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { LoadingService } from '@start9labs/shared'
import {
  TUI_IS_MOBILE,
  TuiAutoFocus,
  tuiMarkControlAsTouchedAndValidate,
  TuiStringHandler,
} from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiError,
  TuiIcon,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiDialog, TuiDialogService } from '@taiga-ui/experimental'
import {
  TUI_CONFIRM,
  TuiCopy,
  TuiDataListWrapper,
  TuiFieldErrorPipe,
  TuiSegmented,
  TuiSelect,
  TuiTextarea,
} from '@taiga-ui/kit'
import { TuiForm, TuiHeader } from '@taiga-ui/layout'
import { QrCodeComponent } from 'ng-qrcode'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData, WgServer } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <table class="g-table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Subnet</th>
          <th>LAN IP</th>
          <th [style.padding-inline-end.rem]="0.625">
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="onAdd()">
              Add
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (device of devices(); track $index) {
          <tr>
            <td>{{ device.name }}</td>
            <td>{{ device.subnet.name }}</td>
            <td>{{ device.ip }}</td>
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
                    (click)="onEdit(device)"
                  >
                    Rename
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.settings"
                    new
                    (click)="onConfig(device)"
                  >
                    View Config
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.trash"
                    new
                    (click)="onDelete(device)"
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
    <ng-template [tuiDialogOptions]="{ label: label() }" [(tuiDialog)]="dialog">
      <form tuiForm [formGroup]="form">
        <tui-textfield>
          <label tuiLabel>Name</label>
          <input tuiTextfield tuiAutoFocus formControlName="name" />
        </tui-textfield>
        <tui-error
          formControlName="name"
          [error]="[] | tuiFieldError | async"
        />

        @if (!editing()) {
          <tui-textfield tuiChevron [stringify]="subnetDisplay">
            <label tuiLabel>Subnet</label>
            @if (mobile) {
              <select
                tuiSelect
                formControlName="subnet"
                [items]="subnets()"
              ></select>
            } @else {
              <input tuiSelect formControlName="subnet" />
            }
            @if (!mobile) {
              <tui-data-list-wrapper
                *tuiTextfieldDropdown
                new
                [items]="subnets()"
              />
            }
          </tui-textfield>
          <tui-error
            formControlName="subnet"
            [error]="[] | tuiFieldError | async"
          />

          @if (form.controls.subnet.value.range) {
            <tui-textfield>
              <label tuiLabel>LAN IP</label>
              <input tuiTextfield tuiAutoFocus formControlName="ip" />
            </tui-textfield>
            <tui-error
              formControlName="ip"
              [error]="[] | tuiFieldError | async"
            />
          }
        }
        <footer>
          <button tuiButton (click)="onSave()" [disabled]="form.invalid">
            Save
          </button>
        </footer>
      </form>
    </ng-template>
    <ng-template [(tuiDialog)]="showConfig">
      <header tuiHeader>
        <h2 tuiTitle>Device Config</h2>
        <aside tuiAccessories>
          <tui-segmented #segmented>
            <button>
              <tui-icon icon="@tui.file" />
              File
            </button>
            <button>
              <tui-icon icon="@tui.qr-code" />
              QR
            </button>
          </tui-segmented>
        </aside>
      </header>
      @if (segmented?.activeItemIndex) {
        <qr-code [value]="config()" size="352" />
      } @else {
        <tui-textfield>
          <textarea
            tuiTextarea
            [min]="16"
            [max]="16"
            [readOnly]="true"
            [value]="config()"
          ></textarea>
          <tui-icon tuiCopy />
          <a
            tuiIconButton
            iconStart="@tui.download"
            download="start-tunnel.conf"
            size="s"
            [href]="href()"
          >
            Download
          </a>
        </tui-textfield>
      }
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiButton,
    TuiDropdown,
    TuiDataList,
    TuiTextfield,
    TuiDialog,
    TuiForm,
    TuiError,
    TuiFieldErrorPipe,
    TuiAutoFocus,
    TuiSelect,
    TuiDataListWrapper,
    TuiHeader,
    TuiTitle,
    TuiSegmented,
    TuiIcon,
    QrCodeComponent,
    TuiTextarea,
    TuiCopy,
  ],
})
export default class Devices {
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)
  private readonly loading = inject(LoadingService)
  private readonly patch = inject<PatchDB<TunnelData>>(PatchDB)

  protected readonly dialog = signal(false)

  protected readonly showConfig = signal(false)
  protected readonly config = signal('')
  protected readonly href = computed(
    () => `data:text/plain;charset=utf-8,${encodeURIComponent(this.config())}`,
  )

  protected readonly editing = signal(false)

  protected readonly subnets = toSignal<MappedSubnet[], []>(
    this.patch.watch$('wg', 'subnets').pipe(
      map(s =>
        Object.entries(s).map(([range, { name, clients }]) => ({
          range,
          name,
          clients,
        })),
      ),
    ),
    { initialValue: [] },
  )

  protected readonly devices = computed(() =>
    this.subnets().flatMap(subnet =>
      Object.entries(subnet.clients).map(([ip, { name }]) => ({
        subnet: {
          name: subnet.name,
          range: subnet.range,
        },
        ip,
        name,
      })),
    ),
  )

  protected subnetDisplay: TuiStringHandler<MappedSubnet> = subnet =>
    subnet.range ? `${subnet.name} (${subnet.range})` : ''

  protected readonly label = computed(() =>
    this.editing() ? 'Rename device' : 'Add device',
  )
  protected readonly mobile = inject(TUI_IS_MOBILE)
  protected readonly form = inject(NonNullableFormBuilder).group({
    name: ['', Validators.required],
    subnet: [{} as MappedDevice['subnet'], Validators.required],
    ip: ['', Validators.required],
  })

  protected onAdd() {
    this.editing.set(false)
    this.form.reset()
    this.dialog.set(true)
  }

  protected onEdit(device: MappedDevice) {
    this.editing.set(true)
    this.form.reset(device)
    this.dialog.set(true)
  }

  async onConfig(device: MappedDevice) {
    const loader = this.loading.open().subscribe()
    try {
      const config = await this.api.showDeviceConfig({
        subnet: device.subnet.range,
        ip: device.ip,
      })

      this.config.set(config)
      this.showConfig.set(true)
    } catch (e) {
      console.log(e)
    } finally {
      loader.unsubscribe()
      this.dialog.set(false)
    }
  }

  protected async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    const loader = this.loading.open().subscribe()

    const { name, subnet, ip } = this.form.getRawValue()
    const toSave = {
      name,
      subnet: subnet.range,
      ip,
    }

    try {
      this.editing()
        ? await this.api.editDevice(toSave)
        : await this.api.addDevice(toSave)
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
      this.dialog.set(false)
    }
  }

  protected onDelete(device: MappedDevice): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loading.open().subscribe()
        try {
          await this.api.deleteDevice({
            subnet: device.subnet.range,
            ip: device.ip,
          })
        } catch (e) {
          console.log(e)
        } finally {
          loader.unsubscribe()
          this.dialog.set(false)
        }
      })
  }
}

type MappedSubnet = {
  range: string
  name: string
  clients: WgServer['subnets']['']['clients']
}

type MappedDevice = {
  subnet: {
    name: string
    range: string
  }
  ip: string
  name: string
}
