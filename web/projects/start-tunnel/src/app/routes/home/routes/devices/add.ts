import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { ErrorService, LoadingService } from '@start9labs/shared'
import {
  TUI_IS_MOBILE,
  TuiAutoFocus,
  tuiMarkControlAsTouchedAndValidate,
} from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiTextfield,
} from '@taiga-ui/core'
import { TuiDialogService } from '@taiga-ui/experimental'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiElasticContainer,
  TuiFieldErrorPipe,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'

import { DEVICES_CONFIG } from './config'
import {
  DeviceData,
  getIp,
  ipInSubnetValidator,
  MappedSubnet,
  subnetValidator,
} from './utils'

@Component({
  template: `
    <form tuiForm [formGroup]="form">
      <tui-textfield>
        <label tuiLabel>Name</label>
        <input tuiTextfield tuiAutoFocus formControlName="name" />
      </tui-textfield>
      <tui-error formControlName="name" [error]="[] | tuiFieldError | async" />

      @if (!context.data.device) {
        <tui-textfield tuiChevron [stringify]="stringify">
          <label tuiLabel>Subnet</label>
          @if (mobile) {
            <select
              tuiSelect
              formControlName="subnet"
              placeholder="Select Subnet"
              [items]="context.data.subnets()"
            ></select>
          } @else {
            <input tuiSelect formControlName="subnet" />
          }
          @if (!mobile) {
            <tui-data-list-wrapper
              *tuiTextfieldDropdown
              new
              [items]="context.data.subnets()"
              (itemClick)="onSubnet($event)"
            />
          }
        </tui-textfield>
        <tui-error
          formControlName="subnet"
          [error]="[] | tuiFieldError | async"
        />

        <tui-elastic-container>
          @if (form.controls.subnet.value?.range) {
            <tui-textfield>
              <label tuiLabel>LAN IP</label>
              <input tuiTextfield tuiAutoFocus formControlName="ip" />
            </tui-textfield>
          }
        </tui-elastic-container>
        @if (form.controls.subnet.value?.range) {
          <tui-error
            formControlName="ip"
            [error]="[] | tuiFieldError | async"
          />
        }
      }
      <footer>
        <button tuiButton (click)="onSave()">Save</button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiAutoFocus,
    TuiButton,
    TuiDataListWrapper,
    TuiError,
    TuiFieldErrorPipe,
    TuiForm,
    TuiSelect,
    TuiTextfield,
    TuiChevron,
    TuiElasticContainer,
  ],
})
export class DevicesAdd {
  private readonly loading = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly dialogs = inject(TuiDialogService)

  protected readonly mobile = inject(TUI_IS_MOBILE)
  protected readonly context =
    injectContext<TuiDialogContext<void, DeviceData>>()

  private readonly autoSubnet =
    !this.context.data.device && this.context.data.subnets().length === 1
      ? this.context.data.subnets().at(0)
      : undefined

  protected readonly form = inject(NonNullableFormBuilder).group({
    name: [this.context.data.device?.name || '', Validators.required],
    subnet: [
      this.context.data.device?.subnet ?? this.autoSubnet,
      [Validators.required, subnetValidator],
    ],
    ip: [
      this.context.data.device?.ip ||
        (this.autoSubnet ? getIp(this.autoSubnet) : ''),
      this.autoSubnet
        ? [Validators.required, ipInSubnetValidator(this.autoSubnet.range)]
        : [],
    ],
  })

  protected readonly stringify = ({ range, name }: MappedSubnet) =>
    range ? `${name} (${range})` : ''

  protected onSubnet(subnet: MappedSubnet) {
    this.form.controls.ip.clearValidators()
    this.form.controls.ip.addValidators([
      Validators.required,
      ipInSubnetValidator(subnet.range),
    ])
    const ip = getIp(subnet)

    if (ip) {
      this.form.controls.ip.setValue(ip)
    } else {
      this.form.controls.ip.disable()
    }

    this.form.controls.subnet.markAsTouched()
  }

  protected async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)

      return
    }

    const loader = this.loading.open().subscribe()
    const { ip, name, subnet } = this.form.getRawValue()
    const data = { ip, name, subnet: subnet?.range || '' }

    try {
      if (this.context.data.device) {
        await this.api.editDevice(data)
      } else {
        await this.api.addDevice(data)

        const config = await this.api.showDeviceConfig({
          subnet: data.subnet,
          ip,
        })

        this.dialogs.open(DEVICES_CONFIG, { data: config }).subscribe()
      }
    } catch (e: any) {
      console.error(e)
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
      this.context.$implicit.complete()
    }
  }
}

export const DEVICES_ADD = new PolymorpheusComponent(DevicesAdd)
