import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
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
  tuiMarkControlAsTouchedAndValidate,
  TuiStringHandler,
} from '@taiga-ui/cdk'
import { TuiButton, TuiError, TuiTextfield } from '@taiga-ui/core'
import { TuiDialog, TuiDialogService } from '@taiga-ui/experimental'
import {
  TUI_CONFIRM,
  TuiChevron,
  TuiDataListWrapper,
  TuiFieldErrorPipe,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { combineLatest, filter, map, Observable } from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <table class="g-table">
      <thead>
        <tr>
          <th>External IP</th>
          <th>External Port</th>
          <th>Device</th>
          <th>Internal Port</th>
          <th [style.padding-inline-end.rem]="0.625">
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="onAdd()">
              Add
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (forward of forwards(); track $index) {
          <tr>
            <td>{{ forward.externalip }}</td>
            <td>{{ forward.externalport }}</td>
            <td>{{ forward.device.name }}</td>
            <td>{{ forward.internalport }}</td>
            <td>
              <button
                tuiIconButton
                size="xs"
                appearance="flat-grayscale"
                iconStart="@tui.trash"
                (click)="onDelete(forward)"
              >
                Actions
              </button>
            </td>
          </tr>
        }
      </tbody>
    </table>
    <ng-template
      [tuiDialogOptions]="{ label: 'Add port forward' }"
      [(tuiDialog)]="dialog"
    >
      <form tuiForm [formGroup]="form">
        <tui-textfield tuiChevron>
          <label tuiLabel>External IP</label>
          @if (mobile) {
            <select
              tuiSelect
              formControlName="externalip"
              [items]="ips"
            ></select>
          } @else {
            <input tuiSelect formControlName="externalip" />
          }
          @if (!mobile) {
            <tui-data-list-wrapper *tuiTextfieldDropdown new [items]="ips" />
          }
        </tui-textfield>
        <tui-error
          formControlName="externalip"
          [error]="[] | tuiFieldError | async"
        />
        <tui-textfield>
          <label tuiLabel>External Port</label>
          <input tuiTextfield formControlName="externalport" />
        </tui-textfield>
        <tui-error
          formControlName="externalport"
          [error]="[] | tuiFieldError | async"
        />
        <tui-textfield tuiChevron [stringify]="deviceDisplay">
          <label tuiLabel>Device</label>
          @if (mobile) {
            <select
              tuiSelect
              formControlName="device"
              [items]="devices()"
            ></select>
          } @else {
            <input tuiSelect formControlName="device" />
          }
          @if (!mobile) {
            <tui-data-list-wrapper
              *tuiTextfieldDropdown
              new
              [items]="devices()"
            />
          }
        </tui-textfield>
        <tui-error
          formControlName="device"
          [error]="[] | tuiFieldError | async"
        />
        <tui-textfield>
          <label tuiLabel>Internal Port</label>
          <input tuiTextfield formControlName="internalport" />
        </tui-textfield>
        <tui-error
          formControlName="internalport"
          [error]="[] | tuiFieldError | async"
        />
        <footer><button tuiButton (click)="onSave()">Save</button></footer>
      </form>
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiButton,
    TuiTextfield,
    TuiDialog,
    TuiForm,
    TuiError,
    TuiFieldErrorPipe,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
  ],
})
export default class PortForwards {
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)
  private readonly loading = inject(LoadingService)
  private readonly patch = inject<PatchDB<TunnelData>>(PatchDB)

  protected readonly dialog = signal(false)

  protected readonly ips = ['69.1.1.42']

  protected readonly devices$: Observable<MappedDevice[]> = this.patch
    .watch$('wg', 'subnets')
    .pipe(
      map(s =>
        Object.values(s).flatMap(({ clients }) =>
          Object.entries(clients).map(([ip, { name }]) => ({
            ip,
            name,
          })),
        ),
      ),
    )

  protected readonly devices = toSignal(this.devices$, {
    initialValue: [],
  })

  protected readonly forwards = toSignal<MappedForward[], []>(
    combineLatest([this.devices$, this.patch.watch$('port_forwards')]).pipe(
      map(([devices, forwards]) =>
        Object.entries(forwards).map(([source, target]) => {
          const sourceSplit = source.split(':')
          const targetSplit = target.split(':')

          return {
            externalip: sourceSplit[0]!,
            externalport: sourceSplit[1]!,
            device: devices.find(d => d.ip === targetSplit[0])!,
            internalport: targetSplit[1]!,
          }
        }),
      ),
    ),
    { initialValue: [] },
  )

  protected readonly deviceDisplay: TuiStringHandler<MappedDevice> = device =>
    device.ip ? `${device.name} (${device.ip})` : ''

  protected readonly mobile = inject(TUI_IS_MOBILE)
  protected readonly form = inject(NonNullableFormBuilder).group({
    externalip: ['', Validators.required],
    externalport: ['', Validators.required],
    device: [{} as MappedDevice, Validators.required],
    internalport: ['', Validators.required],
  })

  protected onAdd(): void {
    this.form.reset()
    this.dialog.set(true)
  }

  protected async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    const loader = this.loading.open().subscribe()

    const { externalip, externalport, device, internalport } =
      this.form.getRawValue()

    try {
      await this.api.addForward({
        source: `${externalip}:${externalport}`,
        target: `${device.ip}:${internalport}`,
      })
    } catch (e) {
      console.error(e)
    } finally {
      loader.unsubscribe()
      this.dialog.set(false)
    }
  }

  protected onDelete(forward: MappedForward): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loading.open().subscribe()
        try {
          await this.api.deleteForward({
            source: `${forward.externalip}:${forward.externalport}`,
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

type MappedDevice = {
  ip: string
  name: string
}

type MappedForward = {
  externalip: string
  externalport: string
  device: MappedDevice
  internalport: string
}
