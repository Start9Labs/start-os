import { Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { ErrorService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiAutoFocus, tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiCheckbox,
  TuiDialogContext,
  TuiError,
  TuiHint,
  TuiIcon,
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

// Wrapper so the Default (ip: null) option survives tuiSelect's null-skipping.
interface WanItem {
  readonly ip: string | null
}

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
        [identityMatcher]="matchWan"
        [stringify]="stringifyWan"
        [tuiTextfieldCleaner]="false"
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

      @if (!context.data.device) {
        <tui-textfield
          tuiChevron
          [stringify]="stringifyKind"
          [tuiTextfieldCleaner]="false"
        >
          <label tuiLabel>Kind</label>
          @if (mobile) {
            <select
              tuiSelect
              formControlName="kind"
              [items]="kindItems"
            ></select>
          } @else {
            <input tuiSelect formControlName="kind" />
          }
          @if (!mobile) {
            <tui-data-list-wrapper *tuiDropdown [items]="kindItems" />
          }
        </tui-textfield>
      }

      <tui-elastic-container>
        @if (!context.data.device && kind() === 'server') {
          <label tuiLabel>
            <input tuiCheckbox type="checkbox" formControlName="dnsInjection" />
            Allow DNS Injection
            <tui-icon
              icon="@tui.info"
              [tuiHint]="dnsInjectionHint"
              [style.cursor]="'help'"
              [style.font-size.rem]="0.9"
            />
          </label>
          <label tuiLabel>
            <input
              tuiCheckbox
              type="checkbox"
              formControlName="autoPortForward"
            />
            Allow Auto Port Forward
            <tui-icon
              icon="@tui.info"
              [tuiHint]="autoPortForwardHint"
              [style.cursor]="'help'"
              [style.font-size.rem]="0.9"
            />
          </label>
        }
      </tui-elastic-container>

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
    TuiHint,
    TuiIcon,
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
    wanIp: this.fb.control<WanItem>({
      ip: this.context.data.device?.wanIp ?? null,
    }),
    kind: this.fb.control<T.Tunnel.WgClientKind>(
      this.context.data.device?.kind ?? 'client',
    ),
    dnsInjection: [this.context.data.device?.allowDnsInjection ?? true],
    autoPortForward: [this.context.data.device?.allowAutoPortForward ?? true],
  })

  protected readonly kind = toSignal(this.form.controls.kind.valueChanges, {
    initialValue: this.form.controls.kind.value,
  })

  protected readonly dnsInjectionHint =
    'The device can add/update the DNS records the tunnel serves for every peer to resolve. Only enable for devices you trust.'
  protected readonly autoPortForwardHint =
    'The device can request port forwards on the gateway (via PCP). Only enable for devices you trust.'

  protected readonly kindItems: readonly T.Tunnel.WgClientKind[] = [
    'client',
    'server',
  ]

  // Real object item so the Default (ip: null) option stringifies in tuiSelect.
  protected readonly wanItems: readonly WanItem[] = [
    { ip: null },
    ...this.context.data.wanOptions.map(ip => ({ ip })),
  ]

  protected readonly stringify = ({ range, name }: MappedSubnet) =>
    range ? `${name} (${range})` : ''
  protected readonly stringifyWan = ({ ip }: WanItem) =>
    wanLabel(ip, this.context.data.defaultWan)
  protected readonly matchWan = (a: WanItem, b: WanItem) => a.ip === b.ip
  protected readonly stringifyKind = (kind: T.Tunnel.WgClientKind) =>
    kind === 'server' ? 'Server' : 'Client'

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
    const { ip, name, subnet, wanIp, kind, dnsInjection, autoPortForward } =
      this.form.getRawValue()
    const data = { ip, name, subnet: subnet?.range || '' }
    const device = this.context.data.device

    try {
      if (device) {
        await this.api.editDevice({ ...data, kind: device.kind })
      } else {
        await this.api.addDevice({ ...data, kind })
      }

      if (wanIp.ip !== (device?.wanIp ?? null)) {
        await this.api.setDeviceWan({
          subnet: data.subnet,
          ip,
          wanIp: wanIp.ip,
        })
      }

      // addDevice sets both flags on for a server; only sync the ones unchecked.
      if (!device && kind === 'server') {
        if (!dnsInjection) {
          await this.api.setDnsInjection({
            subnet: data.subnet,
            ip,
            enabled: false,
          })
        }
        if (!autoPortForward) {
          await this.api.setAutoPortForward({
            subnet: data.subnet,
            ip,
            enabled: false,
          })
        }
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
