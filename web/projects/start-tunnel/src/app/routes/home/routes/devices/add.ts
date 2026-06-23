import { Component, inject } from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { ErrorService } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiAutoFocus, tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiCheckbox,
  TuiDialogContext,
  TuiError,
  TuiInput,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiNotificationMiddleService,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiElasticContainer, TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { wanLabel } from 'src/app/routes/home/components/wan'
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
    <form tuiForm="m" [formGroup]="form">
      <tui-textfield>
        <label tuiLabel>Name</label>
        <input tuiInput tuiAutoFocus formControlName="name" />
      </tui-textfield>
      <tui-error formControlName="name" />

      @if (!context.data.device) {
        <tui-textfield
          tuiChevron
          [stringify]="stringify"
          [tuiTextfieldCleaner]="false"
        >
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
              *tuiDropdown
              [items]="context.data.subnets()"
              (itemClick)="onSubnet($event)"
            />
          }
        </tui-textfield>
        <tui-error formControlName="subnet" />

        <tui-elastic-container>
          @if (form.controls.subnet.value?.range) {
            <tui-textfield>
              <label tuiLabel>LAN IP</label>
              <input tuiInput tuiAutoFocus formControlName="ip" />
            </tui-textfield>
          }
        </tui-elastic-container>
        @if (form.controls.subnet.value?.range) {
          <tui-error formControlName="ip" />
        }
      }

      <tui-textfield
        tuiChevron
        [tuiTextfieldCleaner]="false"
        [stringify]="stringifyWan"
      >
        <label tuiLabel>WAN IP</label>
        @if (mobile) {
          <select tuiSelect formControlName="wanIp" [items]="wanItems"></select>
        } @else {
          <input tuiSelect formControlName="wanIp" />
        }
        @if (!mobile) {
          <tui-data-list-wrapper *tuiDropdown [items]="wanItems" />
        }
      </tui-textfield>

      <label tuiLabel>
        <input
          tuiCheckbox
          type="checkbox"
          formControlName="allowDnsInjection"
        />
        Enable Gateway Autoconfiguration (Recommended for StartOS)
      </label>
      <p
        [style.font-size.rem]="0.8"
        [style.opacity]="0.7"
        [style.margin.rem]="0.25"
      >
        The device will be able to configure the gateway on its own behalf —
        adding and updating the DNS records the tunnel serves and requesting
        port forwards (via PCP). Only enable this for devices you trust.
      </p>

      <footer>
        <button tuiButton (click)="onSave()">Save</button>
      </footer>
    </form>
  `,
  imports: [
    ReactiveFormsModule,
    TuiAutoFocus,
    TuiButton,
    TuiCheckbox,
    TuiDataListWrapper,
    TuiError,
    TuiForm,
    TuiSelect,
    TuiInput,
    TuiChevron,
    TuiElasticContainer,
  ],
})
export class DevicesAdd {
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly dialogs = inject(TuiResponsiveDialogService)

  protected readonly mobile = inject(WA_IS_MOBILE)
  protected readonly context =
    injectContext<TuiDialogContext<void, DeviceData>>()

  private readonly fb = inject(NonNullableFormBuilder)

  private readonly autoSubnet =
    !this.context.data.device && this.context.data.subnets().length === 1
      ? this.context.data.subnets().at(0)
      : undefined

  protected readonly form = this.fb.group({
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
    wanIp: this.fb.control<string | null>(
      this.context.data.device?.wanIp ?? null,
    ),
    allowDnsInjection: [this.context.data.device?.allowDnsInjection ?? false],
  })

  protected readonly wanItems: readonly (string | null)[] = [
    null,
    ...this.context.data.wanOptions,
  ]

  protected readonly stringify = ({ range, name }: MappedSubnet) =>
    range ? `${name} (${range})` : ''
  protected readonly stringifyWan = (ip: string | null) =>
    wanLabel(ip, this.context.data.defaultWan)

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

    const loader = this.loading.open('').subscribe()
    const { ip, name, subnet, wanIp, allowDnsInjection } =
      this.form.getRawValue()
    const data = { ip, name, subnet: subnet?.range || '' }
    const device = this.context.data.device

    try {
      if (device) {
        await this.api.editDevice(data)
      } else {
        await this.api.addDevice(data)
      }

      if (wanIp !== (device?.wanIp ?? null)) {
        await this.api.setDeviceWan({ subnet: data.subnet, ip, wanIp })
      }

      if (allowDnsInjection !== (device?.allowDnsInjection ?? false)) {
        await this.api.setDnsInjection({
          subnet: data.subnet,
          ip,
          enabled: allowDnsInjection,
        })
      }

      if (!device) {
        const config = await this.api.showDeviceConfig({
          subnet: data.subnet,
          ip,
        })

        this.dialogs
          .open(DEVICES_CONFIG, { data: config, closable: false, size: 'm' })
          .subscribe()
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
