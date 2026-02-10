import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiHint,
  TuiLink,
  TuiTitle,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter, from } from 'rxjs'
import { Placeholder } from 'src/app/components/placeholder'
import { Help } from 'src/app/directives/help'
import { OutboundService } from 'src/app/routes/home/routes/outbound/service'
import { ApiService, SecurityProfile } from 'src/app/services/api/api.service'
import { NetworkInterfaceSection, UciFile } from 'src/app/services/api/types'
import { ProfilesAside } from './aside'
import { ADD_PROFILE, ProfileDialogResult } from './dialog'
import { ProfilesService } from './service'

@Component({
  template: `
    <profiles-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Security Profiles</h2></hgroup>
      <aside tuiAccessories>
        <button tuiButton iconStart="@tui.plus" (click)="edit()">Add</button>
      </aside>
    </header>
    <table tuiTable class="g-table" [tuiSkeleton]="!service.data()">
      <thead>
        <tr>
          <th tuiTh [sorter]="'fullname' | tuiSorter">Name</th>
          <th tuiTh>DNS</th>
          <th tuiTh>Outbound</th>
          <th tuiTh>LAN Access</th>
          <th tuiTh>WAN Access</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of tableData() | tuiTableSort; track item.interface) {
          <tr>
            <td tuiTd>{{ item.fullname }}</td>
            <td tuiTd>{{ item.dnsDisplay }}</td>
            <td tuiTd>
              @if (item.outbound === 'wan') {
                {{ item.routingDisplay }}
              } @else {
                <a tuiLink [routerLink]="'/outbound/' + item.outbound">
                  {{ item.routingDisplay }}
                </a>
              }
            </td>
            <td tuiTd>{{ item.lanAccessDisplay }}</td>
            <td tuiTd>{{ item.wanAccessDisplay }}</td>
            <td tuiTd>
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.ellipsis-vertical"
                appearance="icon"
                tuiDropdownAlign="end"
                tuiDropdownAuto
                tuiDropdown
              >
                Actions
                <tui-data-list
                  *tuiDropdown="let close"
                  size="s"
                  (click)="close()"
                >
                  <button
                    tuiOption
                    iconStart="@tui.pencil"
                    (click)="edit(item)"
                  >
                    Edit
                  </button>
                  <button
                    tuiOption
                    class="g-negative"
                    iconStart="@tui.trash"
                    [disabled]="item.owns_lan"
                    [tuiHint]="
                      item.owns_lan
                        ? 'Cannot delete the primary LAN profile'
                        : null
                    "
                    (click)="deleteProfile(item)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        } @empty {
          <tr>
            <td tuiTd colspan="6">
              <app-placeholder icon="@tui.user-lock">
                No security profiles configured
              </app-placeholder>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      max-width: 60rem;
    }

    td:first-child {
      font-weight: bold;
    }

    td:last-child {
      text-align: end;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    TuiHeader,
    TuiTitle,
    TuiButton,
    Help,
    ProfilesAside,
    TuiTable,
    TuiSkeleton,
    Placeholder,
    TuiDataList,
    TuiDropdown,
    TuiHint,
    TuiLink,
    RouterLink,
  ],
})
export default class Profiles {
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(ProfilesService)
  protected readonly outboundService = inject(OutboundService)

  private readonly api = inject(ApiService)

  // Load LAN subnet configuration
  private readonly lanSubnet = toSignal(from(this.loadLanSubnetBase()), {
    initialValue: { firstOctet: 192, secondOctet: 168 },
  })

  protected readonly tableData = computed(() => {
    const profiles = this.service.data()
    const vpns = this.outboundService.data()
    if (!profiles) return []

    return profiles.map(p => ({
      ...p,
      dnsDisplay: this.getDnsDisplay(p),
      routingDisplay: this.getRoutingDisplay(p.outbound, vpns || []),
      lanAccessDisplay: this.getLanAccessDisplay(p.lan_access),
      wanAccessDisplay: this.getWanAccessDisplay(p.wan_access),
    }))
  })

  private async loadLanSubnetBase(): Promise<{
    firstOctet: number
    secondOctet: number
  }> {
    try {
      const uci = await this.api.getUci<{ network: UciFile<any> }>({
        names: ['network'],
      })

      const lanSection = uci.network.sections.find(
        (s): s is NetworkInterfaceSection =>
          s.type === 'interface' && s.name === 'lan',
      )

      if (lanSection?.options?.ipaddr) {
        const [first, second] = lanSection.options.ipaddr.split('.').map(Number)
        return { firstOctet: first || 192, secondOctet: second || 168 }
      }
    } catch (error) {
      console.error('Failed to load LAN subnet configuration:', error)
    }

    return { firstOctet: 192, secondOctet: 168 }
  }

  private getDnsDisplay(profile: SecurityProfile): string {
    return profile.dns_override && profile.dns_override.length
      ? 'Custom'
      : 'System'
  }

  private getRoutingDisplay(
    outbound: string,
    vpns: Array<{ id: string; label: string }>,
  ): string {
    return outbound === 'wan'
      ? 'WAN'
      : (vpns.find(v => v.id === outbound)?.label ?? 'Unknown')
  }

  private getWanAccessDisplay(access: SecurityProfile['wan_access']): string {
    if (access === 'ALL') return 'All'
    if (access === 'NONE') return 'None'

    if (typeof access === 'object' && 'whitelist' in access) {
      return 'Whitelist'
    }

    return typeof access === 'object' && 'blacklist' in access
      ? 'Blacklist'
      : 'Unknown'
  }

  private getLanAccessDisplay(access: SecurityProfile['lan_access']): string {
    if (access === 'ALL') return 'All'
    if (access === 'SAME_PROFILE') return 'Same profile'

    return typeof access === 'object' && 'other_profiles' in access
      ? 'Whitelist'
      : 'Unknown'
  }

  edit(profile?: SecurityProfile) {
    const profiles = this.service.data() || []
    const otherProfiles = profiles.filter(
      p => p.interface !== profile?.interface,
    )
    const usedSubnets = otherProfiles.map(p =>
      parseInt(p.gateway_ip.split('.')[2], 10),
    )
    const vpns = this.outboundService.data() || []
    const outboundVpns = vpns.map(v => ({
      interface: v.id,
      label: v.label,
    }))
    const subnet = this.lanSubnet()

    this.dialogs
      .open<ProfileDialogResult>(ADD_PROFILE, {
        label: profile ? 'Edit Security Profile' : 'Add Security Profile',
        data: {
          existing: profile,
          otherProfiles,
          outboundVpns,
          usedSubnets,
          subnetBase: {
            firstOctet: subnet.firstOctet,
            secondOctet: subnet.secondOctet,
          },
        },
      })
      .subscribe(result => {
        if (profile) {
          this.service.updateProfile({
            ...profile,
            ...result,
          })
        } else {
          this.service.createProfile(result)
        }
      })
  }

  deleteProfile(profile: SecurityProfile) {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.service.deleteProfile({
          fullname: profile.fullname,
          interface: profile.interface,
          vlan_tag: profile.vlan_tag,
        })
      })
  }
}
