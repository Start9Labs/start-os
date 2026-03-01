import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  OnInit,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { TuiContext, tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiInput,
  TuiLabel,
  TuiNotification,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiRadioList,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext } from '@taiga-ui/polymorpheus'
import { ModalHelp } from 'src/app/help/modal-help'
import { provideHelp } from 'src/app/help/help'
import { Device } from 'src/app/routes/devices/utils'
import { Protocol, PublishedPort, PublishedPortDialogResult } from './types'

export interface PublishPortDialogData {
  devices: Device[]
  existing?: PublishedPort
  ipv6Available: boolean
}

const PROTOCOL = { tcp: 'TCP', udp: 'UDP', 'tcp+udp': 'TCP + UDP' }
const PORT: Record<string, string> = { same: 'Same as device', other: 'Other' }
const IP: Record<string, string> = {
  ipv4: 'IPv4',
  ipv6: 'IPv6',
  both: 'IPv4 + IPv6',
}

@Component({
  template: `
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <tui-textfield>
        <label tuiLabel>Label</label>
        <input
          tuiInput
          formControlName="label"
          placeholder="e.g. Home Assistant"
        />
      </tui-textfield>
      <tui-error formControlName="label" />

      <div class="device-port-row">
        <tui-textfield
          tuiChevron
          [tuiTextfieldCleaner]="false"
          [stringify]="stringifyDevice"
        >
          <label tuiLabel>Device</label>
          <input
            tuiSelect
            formControlName="deviceMac"
            placeholder="Select device"
          />
          <tui-data-list-wrapper
            *tuiDropdown
            [items]="deviceMacs()"
            [itemContent]="deviceContent"
          />
          <ng-template #deviceContent let-mac>
            @if (getDeviceInfo(mac); as device) {
              <span tuiTitle>
                {{ device.label }}
                <span tuiSubtitle>{{ device.sublabel }}</span>
              </span>
            }
          </ng-template>
        </tui-textfield>
        <span class="g-secondary">:</span>
        <tui-textfield>
          <label tuiLabel>Port</label>
          <input tuiInput formControlName="ports" placeholder="e.g. 443" />
        </tui-textfield>
      </div>
      <tui-error formControlName="deviceMac" />
      <tui-error formControlName="ports" />

      <fieldset>
        <legend>Protocol</legend>
        <tui-radio-list
          size="s"
          formControlName="protocol"
          [items]="protocolValues"
          [itemContent]="protocol"
        />
      </fieldset>

      <fieldset>
        <legend>Source</legend>
        <tui-radio-list
          size="s"
          formControlName="sourceType"
          [items]="sourceTypeValues"
        />
      </fieldset>

      @if (form.value.sourceType === 'custom') {
        <tui-textfield>
          <label tuiLabel>Source IP / prefix</label>
          <input
            tuiInput
            formControlName="sourceValue"
            placeholder="e.g. 203.0.113.0/24"
          />
        </tui-textfield>
        <tui-error formControlName="sourceValue" />
      }

      <fieldset>
        <legend>IP Version</legend>
        <tui-radio-list
          size="s"
          formControlName="ipVersion"
          [items]="ipVersionValues"
          [itemContent]="ip"
          [disabledItemHandler]="isIpVersionDisabled"
        />
      </fieldset>
      @if (!context.data.ipv6Available) {
        <small class="g-secondary">
          IPv6 options require WAN IPv6 and LAN IPv6 to be enabled
        </small>
      }

      @if (form.value.ipVersion === 'ipv4' || form.value.ipVersion === 'both') {
        <fieldset>
          <legend>IPv4 External Port</legend>
          <tui-radio-list
            size="s"
            formControlName="ipv4PublicPortType"
            [items]="publicPortTypeValues"
            [itemContent]="port"
          />
        </fieldset>
        @if (form.value.ipv4PublicPortType === 'other') {
          <tui-textfield>
            <input
              tuiInput
              formControlName="ipv4PublicPort"
              placeholder="e.g. 8080"
            />
          </tui-textfield>
          <tui-error formControlName="ipv4PublicPort" />
        }
      }

      @if (reservationNeeds()) {
        <div tuiNotification appearance="info">
          Device IPv4 address will be reserved
        </div>
      }

      <footer>
        <button
          tuiButton
          appearance="flat"
          type="button"
          (click)="context.$implicit.complete()"
        >
          Cancel
        </button>
        <button tuiButton>Save</button>
      </footer>
    </form>
  `,
  styles: `
    tui-radio-list {
      flex-direction: row;
      text-transform: capitalize;
    }

    .device-port-row {
      display: grid;
      grid-template-columns: 1fr min-content 8rem;
      gap: 0.5rem;
      line-height: 2rem;
      align-items: flex-end;
    }

    [tuiSubtitle] {
      white-space: pre-line;
    }
  `,
  hostDirectives: [ModalHelp],
  providers: [
    provideHelp('/published-ports/dialog'),
    tuiValidationErrorsProvider({
      required: 'This field is required',
      pattern: 'Invalid format',
    }),
  ],
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiTextfield,
    TuiInput,
    TuiLabel,
    TuiSelect,
    TuiDataListWrapper,
    TuiError,
    TuiButton,
    TuiChevron,
    TuiRadioList,
    TuiNotification,
    TuiTitle,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PublishPortDialog implements OnInit {
  protected readonly context =
    injectContext<
      TuiDialogContext<PublishedPortDialogResult, PublishPortDialogData>
    >()

  // Port pattern: single port (1-65535) or range (port-port)
  private readonly portPattern =
    /^([1-9]\d{0,4}|[1-5]\d{4}|6[0-4]\d{3}|65[0-4]\d{2}|655[0-2]\d|6553[0-5])(-([1-9]\d{0,4}|[1-5]\d{4}|6[0-4]\d{3}|65[0-4]\d{2}|655[0-2]\d|6553[0-5]))?$/
  // CIDR pattern: IP address with optional /prefix
  private readonly cidrPattern = /^(\d{1,3}\.){3}\d{1,3}(\/\d{1,2})?$/

  protected readonly form = inject(NonNullableFormBuilder).group({
    label: ['', Validators.required],
    deviceMac: ['', Validators.required],
    protocol: ['tcp' as Protocol],
    ports: ['', [Validators.required, Validators.pattern(this.portPattern)]],
    sourceType: ['any' as 'any' | 'custom'],
    sourceValue: [''],
    ipVersion: ['both' as 'ipv4' | 'ipv6' | 'both'],
    ipv4PublicPortType: ['same' as 'same' | 'other'],
    ipv4PublicPort: [''],
  })

  // Track form values as signals for computed
  private readonly selectedDeviceMac = toSignal(
    this.form.controls.deviceMac.valueChanges,
    { initialValue: '' },
  )
  private readonly selectedIpVersion = toSignal(
    this.form.controls.ipVersion.valueChanges,
    { initialValue: 'both' as const },
  )

  // Compute if IPv4 reservation is needed
  protected readonly reservationNeeds = computed(() => {
    const mac = this.selectedDeviceMac()
    const ipVersion = this.selectedIpVersion()
    const device = this.deviceMap().get(mac)

    if (!device) return false

    return (ipVersion === 'ipv4' || ipVersion === 'both') && !device.ipv4Static
  })

  protected readonly protocolValues: Protocol[] = ['tcp', 'udp', 'tcp+udp']
  protected readonly sourceTypeValues = ['any', 'custom'] as const
  protected readonly ipVersionValues = ['ipv4', 'ipv6', 'both'] as const
  protected readonly publicPortTypeValues = ['same', 'other'] as const

  protected readonly ip = (ctx: TuiContext<string>) => IP[ctx.$implicit]
  protected readonly port = (ctx: TuiContext<string>) => PORT[ctx.$implicit]
  protected readonly protocol = (ctx: TuiContext<Protocol>) =>
    PROTOCOL[ctx.$implicit]

  protected readonly isIpVersionDisabled = (value: string): boolean =>
    !this.context.data.ipv6Available && (value === 'ipv6' || value === 'both')

  // TODO @Alex refactor this to declarative validation
  ngOnInit() {
    const existing = this.context.data.existing
    const ipv6Available = this.context.data.ipv6Available

    if (existing) {
      const isPublicPortSame =
        !existing.ipv4PublicPort || existing.ipv4PublicPort === existing.ports

      // Convert ipv4/ipv6 booleans to ipVersion
      let ipVersion: 'ipv4' | 'ipv6' | 'both' = 'both'
      if (existing.ipv4 && existing.ipv6) {
        ipVersion = 'both'
      } else if (existing.ipv4) {
        ipVersion = 'ipv4'
      } else {
        ipVersion = 'ipv6'
      }

      // If IPv6 is not available, force to IPv4
      if (!ipv6Available && (ipVersion === 'ipv6' || ipVersion === 'both')) {
        ipVersion = 'ipv4'
      }

      this.form.patchValue({
        label: existing.label,
        deviceMac: existing.deviceMac,
        protocol: existing.protocol,
        ports: existing.ports,
        sourceType: existing.source === 'any' ? 'any' : 'custom',
        sourceValue: existing.source === 'any' ? '' : existing.source,
        ipVersion,
        ipv4PublicPortType: isPublicPortSame ? 'same' : 'other',
        ipv4PublicPort: existing.ipv4PublicPort || '',
      })
    } else {
      // For new ports, default to IPv4 only if IPv6 is not available
      if (!ipv6Available) {
        this.form.patchValue({ ipVersion: 'ipv4' })
      }
    }

    // Update sourceValue validator when sourceType changes
    this.form.controls.sourceType.valueChanges.subscribe(type => {
      const sourceValueControl = this.form.controls.sourceValue
      if (type === 'custom') {
        sourceValueControl.setValidators([
          Validators.required,
          Validators.pattern(this.cidrPattern),
        ])
      } else {
        sourceValueControl.clearValidators()
      }
      sourceValueControl.updateValueAndValidity()
    })

    // Update ipv4PublicPort validator when ipv4PublicPortType or ipVersion changes
    const updatePublicPortValidation = () => {
      const publicPortControl = this.form.controls.ipv4PublicPort
      const ipVersion = this.form.value.ipVersion
      const portType = this.form.value.ipv4PublicPortType
      const needsValidation =
        (ipVersion === 'ipv4' || ipVersion === 'both') && portType === 'other'

      if (needsValidation) {
        publicPortControl.setValidators([
          Validators.required,
          Validators.pattern(this.portPattern),
        ])
      } else {
        publicPortControl.clearValidators()
      }
      publicPortControl.updateValueAndValidity()
    }

    this.form.controls.ipv4PublicPortType.valueChanges.subscribe(
      updatePublicPortValidation,
    )
    this.form.controls.ipVersion.valueChanges.subscribe(
      updatePublicPortValidation,
    )

    // Trigger initial validation based on current values
    if (this.form.value.sourceType === 'custom') {
      this.form.controls.sourceValue.setValidators([
        Validators.required,
        Validators.pattern(this.cidrPattern),
      ])
      this.form.controls.sourceValue.updateValueAndValidity()
    }
    updatePublicPortValidation()
  }

  protected readonly deviceMacs = computed(() => {
    return this.context.data.devices
      .filter(d => d.status !== 'blocked')
      .map(d => d.mac)
  })

  protected readonly deviceMap = computed(() => {
    const map = new Map<string, Device>()
    for (const device of this.context.data.devices) {
      map.set(device.mac, device)
    }
    return map
  })

  protected readonly stringifyDevice = (mac: string) => {
    const device = this.deviceMap().get(mac)
    return device?.name || device?.hostname || mac
  }

  protected getDeviceInfo(
    mac: string,
  ): { label: string; sublabel: string } | undefined {
    const device = this.deviceMap().get(mac)
    if (!device) return undefined

    return {
      label: device.name || device.hostname,
      sublabel: [device.ipv4, mac].filter(Boolean).join('\n'),
    }
  }

  protected save() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    const value = this.form.getRawValue()
    const existing = this.context.data.existing

    // Convert ipVersion to ipv4/ipv6 booleans
    const ipv4 = value.ipVersion === 'ipv4' || value.ipVersion === 'both'
    const ipv6 = value.ipVersion === 'ipv6' || value.ipVersion === 'both'

    // Determine IPv4 public port based on selection
    let ipv4PublicPort: string | undefined
    if (ipv4) {
      ipv4PublicPort =
        value.ipv4PublicPortType === 'same'
          ? value.ports
          : value.ipv4PublicPort || value.ports
    }

    const port: PublishedPort = {
      id: existing?.id || crypto.randomUUID(),
      enabled: existing?.enabled ?? true,
      label: value.label,
      deviceMac: value.deviceMac,
      ports: value.ports,
      protocol: value.protocol,
      ipv4,
      ipv6,
      ipv4PublicPort,
      source: value.sourceType === 'any' ? 'any' : value.sourceValue,
    }

    const result: PublishedPortDialogResult = {
      port,
      reserveIpv4: this.reservationNeeds(),
    }

    this.context.completeWith(result)
  }
}
