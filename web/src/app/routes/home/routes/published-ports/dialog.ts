import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  OnInit,
} from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiInput,
  TuiLabel,
  TuiTextfield,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiRadioList,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiForm, TuiHeader } from '@taiga-ui/layout'
import { injectContext } from '@taiga-ui/polymorpheus'
import { Device } from 'src/app/routes/home/routes/devices/utils'
import { Protocol, PublishedPort } from './types'

export interface PublishPortDialogData {
  devices: Device[]
  existing?: PublishedPort
}

@Component({
  template: `
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <tui-textfield class="field-narrow">
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
              <span class="device-option">
                <span>{{ device.label }}</span>
                <span class="g-secondary">{{ device.sublabel }}</span>
              </span>
            }
          </ng-template>
        </tui-textfield>
        <span class="colon-separator">:</span>
        <tui-textfield class="port-field">
          <label tuiLabel>Port(s)</label>
          <input tuiInput formControlName="ports" placeholder="e.g. 443" />
        </tui-textfield>
      </div>
      <tui-error formControlName="deviceMac" />
      <tui-error formControlName="ports" />

      <h3 tuiHeader="body-m">Protocol</h3>
      <tui-radio-list
        size="s"
        formControlName="protocol"
        [items]="protocolValues"
        [itemContent]="protocolContent"
        [style.flex-direction]="'row'"
      />
      <ng-template #protocolContent let-value>
        {{ getProtocolLabel(value) }}
      </ng-template>

      <h3 tuiHeader="body-m">Source</h3>
      <tui-radio-list
        size="s"
        formControlName="sourceType"
        [items]="sourceTypeValues"
        [itemContent]="sourceContent"
        [style.flex-direction]="'row'"
      />
      <ng-template #sourceContent let-value>
        {{ getSourceTypeLabel(value) }}
      </ng-template>
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

      <h3 tuiHeader="body-m">IP Version</h3>
      <tui-radio-list
        size="s"
        formControlName="ipVersion"
        [items]="ipVersionValues"
        [itemContent]="ipVersionContent"
        [style.flex-direction]="'row'"
      />
      <ng-template #ipVersionContent let-value>
        {{ getIpVersionLabel(value) }}
      </ng-template>

      @if (form.value.ipVersion === 'ipv4' || form.value.ipVersion === 'both') {
        <h3 tuiHeader="body-m">IPv4 External Port(s)</h3>
        <tui-radio-list
          size="s"
          formControlName="ipv4PublicPortType"
          [items]="publicPortTypeValues"
          [itemContent]="publicPortTypeContent"
          [style.flex-direction]="'row'"
        />
        <ng-template #publicPortTypeContent let-value>
          {{ getPublicPortTypeLabel(value) }}
        </ng-template>
        @if (form.value.ipv4PublicPortType === 'other') {
          <tui-textfield class="public-port-input">
            <input
              tuiInput
              formControlName="ipv4PublicPort"
              placeholder="e.g. 8080"
            />
          </tui-textfield>
          <tui-error formControlName="ipv4PublicPort" />
        }
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
        <button tuiButton [disabled]="form.invalid">Save</button>
      </footer>
    </form>
  `,
  styles: `
    h3 {
      font-weight: bold;
    }

    .field-narrow {
      max-width: 23rem;
    }

    .device-port-row {
      display: flex;
      gap: 0.5rem;
      align-items: flex-end;

      > tui-textfield:first-child {
        flex: 1;
        max-width: 14rem;
      }
    }

    .colon-separator {
      padding-bottom: 0.5rem;
      color: var(--tui-text-secondary);
    }

    .port-field {
      width: 8rem;
    }

    .device-option {
      display: flex;
      flex-direction: column;
    }

    .public-port-input {
      max-width: 10rem;
    }
  `,
  providers: [
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
    TuiHeader,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PublishPortDialog implements OnInit {
  protected readonly context =
    injectContext<TuiDialogContext<PublishedPort, PublishPortDialogData>>()

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

  protected readonly protocolValues: Protocol[] = ['tcp', 'udp', 'tcp+udp']
  private readonly protocolLabels: Record<Protocol, string> = {
    tcp: 'TCP',
    udp: 'UDP',
    'tcp+udp': 'TCP + UDP',
  }

  protected readonly sourceTypeValues = ['any', 'custom'] as const
  private readonly sourceTypeLabels: Record<string, string> = {
    any: 'Any',
    custom: 'Custom',
  }

  protected readonly ipVersionValues = ['ipv4', 'ipv6', 'both'] as const
  private readonly ipVersionLabels: Record<string, string> = {
    ipv4: 'IPv4',
    ipv6: 'IPv6',
    both: 'IPv4 + IPv6',
  }

  protected readonly publicPortTypeValues = ['same', 'other'] as const
  private readonly publicPortTypeLabels: Record<string, string> = {
    same: 'Same as device',
    other: 'Other',
  }

  ngOnInit() {
    const existing = this.context.data.existing
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

  protected getProtocolLabel(value: string): string {
    return this.protocolLabels[value as Protocol] || value
  }

  protected getSourceTypeLabel(value: string): string {
    return this.sourceTypeLabels[value] || value
  }

  protected getIpVersionLabel(value: string): string {
    return this.ipVersionLabels[value] || value
  }

  protected getPublicPortTypeLabel(value: string): string {
    return this.publicPortTypeLabels[value] || value
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

    const parts: string[] = []
    if (device.connection) parts.push(device.connection)
    if (device.ipv4) parts.push(device.ipv4)

    return {
      label: device.name || device.hostname,
      sublabel: parts.join(' • '),
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

    const result: PublishedPort = {
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

    this.context.completeWith(result)
  }
}
