import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  FormsModule,
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import {
  TuiContext,
  tuiMarkControlAsTouchedAndValidate,
  TuiStringHandler,
} from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiCheckbox,
  TuiDataList,
  TuiDialogContext,
  TuiError,
  TuiGroup,
  TuiInput,
  TuiLabel,
  TuiSelectLike,
  TuiTextfield,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiInputChip,
  TuiInputNumber,
  TuiMultiSelect,
  TuiRadioList,
  TuiSelect,
  TuiStringifyPipe,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { startWith } from 'rxjs'
import { provideHelp } from 'src/app/help/help'
import { ModalHelp } from 'src/app/help/modal-help'
import type {
  LanAccess,
  ProfileIdOpt,
  SecurityProfile,
  WanAccess,
} from 'src/app/services/api/api.service'
import { CustomValidators } from 'src/app/utils/validators'

export interface ProfileDialogData {
  existing?: SecurityProfile
  otherProfiles: SecurityProfile[]
  outboundVpns: Array<{ interface: string; label: string }>
  usedSubnets: number[]
  subnetBase: { firstOctet: number; secondOctet: number }
}

export interface ProfileDialogResult {
  fullname?: string
  gateway_ip: string
  outbound: string
  lan_access: LanAccess<ProfileIdOpt>
  wan_access: WanAccess
  access_to_new_profiles: boolean
  owns_lan: boolean
  dns_override?: string[]
}

@Component({
  template: `
    <form
      tuiForm="m"
      class="g-form"
      [formGroup]="form"
      (submit.prevent)="save()"
    >
      <tui-textfield>
        <label tuiLabel>Name</label>
        <input tuiInput formControlName="fullname" placeholder="e.g. Guest" />
      </tui-textfield>
      <tui-error formControlName="fullname" />
      <fieldset>
        <legend>Subnet</legend>
        <div tuiGroup>
          <tui-textfield>
            <input tuiInput [value]="subnetBase().firstOctet" disabled />
          </tui-textfield>
          <tui-textfield>
            <input tuiInput [value]="subnetBase().secondOctet" disabled />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input
              tuiInputNumber
              formControlName="subnet"
              [min]="1"
              [max]="254"
            />
          </tui-textfield>
          <tui-textfield>
            <input tuiInput value="1" disabled />
          </tui-textfield>
        </div>
      </fieldset>
      <tui-error formControlName="subnet" />
      <tui-textfield
        tuiChevron
        [tuiTextfieldCleaner]="false"
        [stringify]="stringifyOutbound"
      >
        <label tuiLabel>Outbound Routing</label>
        <input tuiSelect formControlName="outbound" />
        <tui-data-list *tuiDropdown>
          @for (option of outboundOptions(); track option.interface) {
            <button tuiOption [value]="option.interface">
              {{ option.label }}
            </button>
          }
        </tui-data-list>
      </tui-textfield>
      <label tuiLabel>
        <input type="checkbox" tuiSwitch formControlName="useCustomDns" />
        Use custom DNS servers
      </label>
      @if (useCustomDns()) {
        <section>
          <tui-textfield>
            <label tuiLabel>Primary*</label>
            <input tuiInput formControlName="dns1" />
          </tui-textfield>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="dns1Tls" />
            TLS
          </label>
        </section>
        <tui-error formControlName="dns1" />
        <section>
          <tui-textfield>
            <label tuiLabel>Secondary</label>
            <input tuiInput formControlName="dns2" />
          </tui-textfield>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="dns2Tls" />
            TLS
          </label>
        </section>
        <tui-error formControlName="dns2" />
        <section>
          <tui-textfield>
            <label tuiLabel>Tertiary</label>
            <input tuiInput formControlName="dns3" />
          </tui-textfield>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="dns3Tls" />
            TLS
          </label>
        </section>
        <tui-error formControlName="dns3" />
      }

      <fieldset>
        <legend>LAN Access</legend>
        <tui-radio-list
          size="s"
          formControlName="lanAccessType"
          [items]="lanAccessOptions"
          [itemContent]="getLanAccessLabel"
          [style.flex-direction]="'row'"
        />
      </fieldset>

      <!-- LAN Access Profile Checkboxes (conditional) -->
      @if (lanAccessType() === 'whitelist' || lanAccessType() === 'blacklist') {
        <tui-textfield multi tuiChevron [stringify]="'fullname' | tuiStringify">
          @if (!context.data.otherProfiles.length) {
            <label tuiLabel>No other profiles</label>
          }
          <input
            tuiInputChip
            tuiSelectLike
            [disabled]="!context.data.otherProfiles.length"
            [placeholder]="multi().length ? '' : 'Profiles'"
            [ngModelOptions]="{ standalone: true }"
            [(ngModel)]="multi"
            (ngModelChange)="onWhitelist($event)"
          />
          <tui-input-chip *tuiItem />
          <tui-data-list-wrapper
            *tuiDropdown
            tuiMultiSelectGroup
            [items]="context.data.otherProfiles"
          />
        </tui-textfield>

        <label tuiLabel>
          <input
            type="checkbox"
            tuiCheckbox
            formControlName="accessToNewProfiles"
          />
          Auto whitelist new profiles
        </label>
      }

      <fieldset>
        <legend>WAN Access</legend>
        <tui-radio-list
          size="s"
          formControlName="wanAccessType"
          [items]="wanAccessOptions"
          [itemContent]="getWanAccessLabel"
          [style.flex-direction]="'row'"
        />
      </fieldset>

      @if (wanAccessType() === 'whitelist' || wanAccessType() === 'blacklist') {
        <tui-textfield>
          @if (wanAccessType() === 'whitelist') {
            <label tuiLabel>Allowed Destinations</label>
          } @else {
            <label tuiLabel>Blocked Destinations</label>
          }
          <input
            tuiInput
            formControlName="wanAccessList"
            placeholder="e.g. 1.1.1.1, 8.8.8.8/24"
          />
        </tui-textfield>
        <tui-error formControlName="wanAccessList" />
      }

      <footer>
        <button
          tuiButton
          type="button"
          appearance="flat"
          (click)="context.$implicit.complete()"
        >
          Cancel
        </button>
        <button tuiButton>Save</button>
      </footer>
    </form>
  `,
  hostDirectives: [ModalHelp],
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [
    provideHelp('/profiles/dialog'),
    tuiValidationErrorsProvider({
      required: 'Required',
      min: 'Must be at least 1',
      max: 'Must be at most 254',
      ipv4: 'Enter a valid IPv4 address',
      ipv4List:
        'Enter valid IPv4 addresses or CIDRs (e.g. 1.1.1.1, 8.8.8.8/24)',
    }),
  ],
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiTextfield,
    TuiError,
    TuiButton,
    TuiInput,
    TuiLabel,
    TuiCheckbox,
    TuiRadioList,
    TuiInputNumber,
    TuiSelect,
    TuiChevron,
    TuiDataList,
    TuiSwitch,
    TuiGroup,
    TuiSelectLike,
    TuiInputChip,
    TuiMultiSelect,
    TuiDataListWrapper,
    FormsModule,
    TuiStringifyPipe,
  ],
})
class AddProfile {
  protected readonly context =
    injectContext<TuiDialogContext<ProfileDialogResult, ProfileDialogData>>()

  private readonly builder = inject(NonNullableFormBuilder)
  private readonly existing = this.context.data.existing
  private readonly usedSubnets = this.context.data.usedSubnets

  protected readonly subnetBase = computed(() => this.context.data.subnetBase)

  private readonly dnsConfig = this.parseDnsOverride()

  protected readonly form = this.builder.group({
    fullname: [this.existing?.fullname || '', Validators.required],
    subnet: [
      this.nextAvailableSubnet(),
      [Validators.required, Validators.min(1), Validators.max(254)],
    ],
    outbound: [this.getOutbound()],
    useCustomDns: [this.dnsConfig.useCustomDns],
    dns1: [this.dnsConfig.dns1, CustomValidators.ipv4()],
    dns1Tls: [this.dnsConfig.dns1Tls],
    dns2: [this.dnsConfig.dns2, CustomValidators.ipv4()],
    dns2Tls: [this.dnsConfig.dns2Tls],
    dns3: [this.dnsConfig.dns3, CustomValidators.ipv4()],
    dns3Tls: [this.dnsConfig.dns3Tls],
    lanAccessType: [this.getLanAccessType()],
    lanAccessProfiles: this.builder.group(this.buildLanAccessProfiles()),
    wanAccessType: [this.getWanAccessType()],
    wanAccessList: [this.getWanAccessList(), CustomValidators.ipv4List()],
    accessToNewProfiles: [this.existing?.access_to_new_profiles || false],
  })

  protected readonly multi = signal(
    this.context.data.otherProfiles.filter(
      profile =>
        this.form.controls.lanAccessProfiles.controls[profile.interface]?.value,
    ),
  )

  protected readonly lanAccessType = toSignal(
    this.form.controls.lanAccessType.valueChanges.pipe(
      startWith(this.form.controls.lanAccessType.value),
    ),
    { requireSync: true },
  )

  protected readonly wanAccessType = toSignal(
    this.form.controls.wanAccessType.valueChanges.pipe(
      startWith(this.form.controls.wanAccessType.value),
    ),
    { requireSync: true },
  )

  protected readonly useCustomDns = toSignal(
    this.form.controls.useCustomDns.valueChanges.pipe(
      startWith(this.form.controls.useCustomDns.value),
    ),
    { requireSync: true },
  )

  protected readonly outboundOptions = computed(() => {
    return [
      { interface: 'wan', label: 'WAN' },
      ...this.context.data.outboundVpns,
    ]
  })

  protected readonly lanAccessOptions = ['all', 'none', 'whitelist']
  protected readonly wanAccessOptions = [
    'all',
    'none',
    'whitelist',
    'blacklist',
  ]

  protected readonly getLanAccessLabel: TuiStringHandler<TuiContext<string>> =
    ({ $implicit }) => {
      switch ($implicit) {
        case 'all':
          return 'All'
        case 'none':
          return 'Same profile'
        case 'whitelist':
          return 'Whitelist'
        default:
          return $implicit
      }
    }

  protected readonly getWanAccessLabel: TuiStringHandler<TuiContext<string>> =
    ({ $implicit }) =>
      $implicit.charAt(0).toUpperCase() + $implicit.slice(1).toLowerCase()

  protected onWhitelist(list: readonly SecurityProfile[]) {
    Object.entries(this.form.controls.lanAccessProfiles.controls).forEach(
      ([key, control]) => {
        control.setValue(list.some(item => item.interface === key))
      },
    )
  }

  private nextAvailableSubnet(): number {
    if (this.existing) {
      return parseInt(this.existing.gateway_ip.split('.')[2], 10)
    }
    for (let i = 1; i <= 254; i++) {
      if (!this.usedSubnets.includes(i)) return i
    }
    return 1
  }

  private getOutbound(): string {
    // TODO: Add outbound field to backend SecurityProfile type
    return this.existing?.outbound || 'wan'
  }

  private parseDnsOverride(): {
    useCustomDns: boolean
    dns1: string
    dns1Tls: boolean
    dns2: string
    dns2Tls: boolean
    dns3: string
    dns3Tls: boolean
  } {
    const servers = this.existing?.dns_override || []
    if (servers.length === 0) {
      return {
        useCustomDns: false,
        dns1: '',
        dns1Tls: false,
        dns2: '',
        dns2Tls: false,
        dns3: '',
        dns3Tls: false,
      }
    }

    const parse = (server: string): { ip: string; tls: boolean } =>
      server.includes('@853') || server.includes('#853')
        ? { ip: server.split(/[@#]/)[0], tls: true }
        : { ip: server, tls: false }

    const s1 = servers[0] ? parse(servers[0]) : { ip: '', tls: false }
    const s2 = servers[1] ? parse(servers[1]) : { ip: '', tls: false }
    const s3 = servers[2] ? parse(servers[2]) : { ip: '', tls: false }

    return {
      useCustomDns: true,
      dns1: s1.ip,
      dns1Tls: s1.tls,
      dns2: s2.ip,
      dns2Tls: s2.tls,
      dns3: s3.ip,
      dns3Tls: s3.tls,
    }
  }

  private getLanAccessType(): string {
    if (!this.existing) return 'all'
    const access = this.existing.lan_access
    if (access === 'ALL') return 'all'
    if (access === 'SAME_PROFILE') return 'none'
    if (typeof access === 'object' && 'other_profiles' in access) {
      return 'whitelist'
    }
    return 'all'
  }

  private buildLanAccessProfiles(): Record<string, boolean> {
    const result: Record<string, boolean> = {}
    const selectedProfiles = this.existing?.lan_access
    const selected =
      typeof selectedProfiles === 'object' &&
      'other_profiles' in selectedProfiles
        ? selectedProfiles.other_profiles
        : []

    for (const profile of this.context.data.otherProfiles) {
      // Check if this profile is in the selected list by matching interface
      result[profile.interface] = selected.some(
        (s: any) => s.interface === profile.interface,
      )
    }
    return result
  }

  private getWanAccessType(): string {
    if (!this.existing) return 'all'
    const access = this.existing.wan_access
    if (access === 'ALL') return 'all'
    if (access === 'NONE') return 'none'
    if (typeof access === 'object' && 'whitelist' in access) return 'whitelist'
    if (typeof access === 'object' && 'blacklist' in access) return 'blacklist'
    return 'all'
  }

  private getWanAccessList(): string {
    if (!this.existing) return ''
    const access = this.existing.wan_access
    if (typeof access === 'object' && 'whitelist' in access) {
      return access.whitelist.join(', ')
    }
    if (typeof access === 'object' && 'blacklist' in access) {
      return access.blacklist.join(', ')
    }
    return ''
  }

  protected readonly stringifyOutbound = (value: string): string => {
    const option = this.outboundOptions().find(o => o.interface === value)
    return option?.label ?? value
  }

  protected save(): void {
    tuiMarkControlAsTouchedAndValidate(this.form)

    if (this.form.valid) {
      const val = this.form.getRawValue()

      // Build LAN access
      let lan_access: LanAccess<ProfileIdOpt>
      if (val.lanAccessType === 'all') {
        lan_access = 'ALL'
      } else if (val.lanAccessType === 'none') {
        lan_access = 'SAME_PROFILE'
      } else {
        // whitelist
        const selectedProfiles = this.context.data.otherProfiles
          .filter(p => val.lanAccessProfiles[p.interface])
          .map(p => ({
            fullname: p.fullname,
            interface: p.interface,
            vlan_tag: p.vlan_tag,
          }))
        lan_access = { other_profiles: selectedProfiles }
      }

      // Build WAN access
      let wan_access: WanAccess
      if (val.wanAccessType === 'all') {
        wan_access = 'ALL'
      } else if (val.wanAccessType === 'none') {
        wan_access = 'NONE'
      } else if (val.wanAccessType === 'whitelist') {
        const ips = val.wanAccessList
          .split(',')
          .map(s => s.trim())
          .filter(Boolean)
        wan_access = { whitelist: ips }
      } else {
        // blacklist
        const ips = val.wanAccessList
          .split(',')
          .map(s => s.trim())
          .filter(Boolean)
        wan_access = { blacklist: ips }
      }

      // Build DNS override
      let dns_override: string[] | undefined
      if (val.useCustomDns) {
        const servers: string[] = []
        if (val.dns1) {
          servers.push(val.dns1Tls ? `${val.dns1}@853` : val.dns1)
        }
        if (val.dns2) {
          servers.push(val.dns2Tls ? `${val.dns2}@853` : val.dns2)
        }
        if (val.dns3) {
          servers.push(val.dns3Tls ? `${val.dns3}@853` : val.dns3)
        }
        dns_override = servers.length ? servers : undefined
      }

      const subnet = this.subnetBase()
      this.context.completeWith({
        fullname: val.fullname,
        gateway_ip: `${subnet.firstOctet}.${subnet.secondOctet}.${val.subnet}.1`,
        outbound: val.outbound,
        lan_access,
        wan_access,
        access_to_new_profiles: val.accessToNewProfiles,
        owns_lan: this.existing?.owns_lan || false,
        dns_override: dns_override?.length ? dns_override : undefined,
      })
    }
  }
}

export const ADD_PROFILE = new PolymorpheusComponent(AddProfile)
