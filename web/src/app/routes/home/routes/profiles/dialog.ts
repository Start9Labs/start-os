import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDataList,
  TuiDialogContext,
  TuiError,
  TuiInput,
  TuiLabel,
  TuiNotification,
  TuiOption,
  TuiTextfield,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import {
  TuiCheckbox,
  TuiChevron,
  TuiInputNumber,
  TuiRadioList,
  TuiSelect,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiForm, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent, injectContext } from '@taiga-ui/polymorpheus'
import { startWith } from 'rxjs'
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
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <!-- Name -->
      <h3 tuiHeader="body-m">Name</h3>
      <tui-textfield class="field-narrow">
        <input tuiInput formControlName="fullname" placeholder="e.g. Guest" />
      </tui-textfield>
      <tui-error formControlName="fullname" />

      <!-- Subnet -->
      <h3 tuiHeader="body-m">Subnet</h3>
      <div class="ip-group">
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
      <tui-error formControlName="subnet" />

      <!-- Outbound Routing -->
      <h3 tuiHeader="body-m">Outbound Routing</h3>
      <tui-textfield
        class="field-narrow"
        tuiChevron
        [tuiTextfieldCleaner]="false"
        [stringify]="stringifyOutbound"
      >
        <input tuiSelect formControlName="outbound" />
        <tui-data-list *tuiDropdown>
          @for (option of outboundOptions(); track option.interface) {
            <button tuiOption [value]="option.interface">
              {{ option.label }}
            </button>
          }
        </tui-data-list>
      </tui-textfield>

      <!-- DNS Override -->
      <h3 tuiHeader="body-m">DNS</h3>
      <label tuiLabel>
        <input type="checkbox" tuiSwitch formControlName="useCustomDns" />
        Use custom DNS servers
      </label>
      @if (useCustomDns()) {
        <section>
          <div>
            <tui-textfield class="field-narrow">
              <label tuiLabel>Primary*</label>
              <input tuiInput formControlName="dns1" />
            </tui-textfield>
            <tui-error formControlName="dns1" />
          </div>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="dns1Tls" />
            TLS
          </label>
        </section>
        <section>
          <div>
            <tui-textfield class="field-narrow">
              <label tuiLabel>Secondary</label>
              <input tuiInput formControlName="dns2" />
            </tui-textfield>
            <tui-error formControlName="dns2" />
          </div>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="dns2Tls" />
            TLS
          </label>
        </section>
        <section>
          <div>
            <tui-textfield class="field-narrow">
              <label tuiLabel>Tertiary</label>
              <input tuiInput formControlName="dns3" />
            </tui-textfield>
            <tui-error formControlName="dns3" />
          </div>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="dns3Tls" />
            TLS
          </label>
        </section>
      }

      <!-- LAN Access -->
      <h3 tuiHeader="body-m">LAN Access</h3>
      <tui-radio-list
        size="s"
        formControlName="lanAccessType"
        [items]="lanAccessOptions"
        [itemContent]="lanAccessContent"
        [style.flex-direction]="'row'"
      />
      <ng-template #lanAccessContent let-value>
        {{ getLanAccessLabel(value) }}
      </ng-template>

      <!-- LAN Access Profile Checkboxes (conditional) -->
      @if (lanAccessType() === 'whitelist' || lanAccessType() === 'blacklist') {
        <div
          class="profile-checkboxes"
          [formGroup]="form.controls.lanAccessProfiles"
        >
          @for (profile of otherProfiles(); track profile.interface) {
            <label tuiLabel>
              <input
                type="checkbox"
                tuiCheckbox
                [formControlName]="profile.interface"
              />
              {{ profile.fullname }}
            </label>
          } @empty {
            <span>No other profiles</span>
          }
        </div>

        <!-- Access to New Profiles -->
        <label tuiLabel>
          <input
            type="checkbox"
            tuiCheckbox
            formControlName="accessToNewProfiles"
          />
          Auto whitelist new profiles
        </label>
      }

      <!-- WAN Access -->
      <h3 tuiHeader="body-m">WAN Access</h3>
      <tui-radio-list
        size="s"
        formControlName="wanAccessType"
        [items]="wanAccessOptions"
        [itemContent]="wanAccessContent"
        [style.flex-direction]="'row'"
      />
      <ng-template #wanAccessContent let-value>
        {{ getWanAccessLabel(value) }}
      </ng-template>

      <!-- WAN Access IP List (conditional) -->
      @if (wanAccessType() === 'whitelist' || wanAccessType() === 'blacklist') {
        <tui-textfield>
          <label tuiLabel>
            {{
              wanAccessType() === 'whitelist'
                ? 'Allowed Destinations'
                : 'Blocked Destinations'
            }}
          </label>
          <input
            tuiInput
            formControlName="wanAccessList"
            placeholder="e.g. 1.1.1.1, 8.8.8.8/24"
          />
        </tui-textfield>
        <tui-error formControlName="wanAccessList" />
      }

      <!-- Footer -->
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
  styles: `
    .field-narrow {
      max-width: 21rem;
    }

    h3 {
      font-weight: bold;
      margin-top: 1rem;
    }

    .ip-group {
      display: flex;
      gap: 0.25rem;
      align-items: center;

      tui-textfield {
        width: 4.5rem;
      }
    }

    .profile-checkboxes {
      display: flex;
      flex-direction: column;
      gap: 0.5rem;
      margin-top: 0.5rem;
      padding: 0.5rem;
      border: 1px solid var(--tui-border-normal);
      border-radius: var(--tui-radius-m);
    }

    .hint {
      margin-top: 0.25rem;
      font-size: 0.875rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [
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
    TuiHeader,
    TuiNotification,
    TuiSelect,
    TuiChevron,
    TuiDataList,
    TuiOption,
    TuiSwitch,
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

  protected readonly otherProfiles = computed(
    () => this.context.data.otherProfiles,
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

    const parse = (server: string): { ip: string; tls: boolean } => {
      if (server.includes('@853') || server.includes('#853')) {
        return { ip: server.split(/[@#]/)[0], tls: true }
      }
      return { ip: server, tls: false }
    }

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

  protected getLanAccessLabel(value: string): string {
    switch (value) {
      case 'all':
        return 'All'
      case 'none':
        return 'Same profile'
      case 'whitelist':
        return 'Whitelist'
      default:
        return value
    }
  }

  protected getWanAccessLabel(value: string): string {
    return value.charAt(0).toUpperCase() + value.slice(1).toLowerCase()
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
        const selectedProfiles = this.otherProfiles()
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
