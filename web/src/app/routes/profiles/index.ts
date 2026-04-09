import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { RouterLink, Routes } from '@angular/router'
import { WA_WINDOW } from '@ng-web-apis/common'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiHint,
  TuiLink,
  TuiNotificationService,
  TuiTitle,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { catchError, EMPTY, filter, firstValueFrom } from 'rxjs'
import { Placeholder } from 'src/app/components/placeholder'
import { ReconnectingDialog } from 'src/app/components/reconnecting-dialog'
import { OutboundService } from 'src/app/routes/outbound/service'
import { SecurityProfile } from 'src/app/services/api/api.service'
import { NetworkRestartService } from 'src/app/services/network-restart.service'
import { ADD_PROFILE, ProfileDialogResult } from './dialog'
import { ProfilesService } from './service'

@Component({
  template: `
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
                <a
                  tuiLink
                  routerLink="/outbound/vpn"
                  [queryParams]="{ id: item.outbound }"
                >
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
                    iconStart="@tui.clock"
                    [routerLink]="[item.interface, 'schedule']"
                  >
                    WAN Schedule
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
class Profiles {
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(ProfilesService)
  protected readonly outboundService = inject(OutboundService)
  private readonly alerts = inject(TuiNotificationService)
  private readonly networkRestart = inject(NetworkRestartService)
  private readonly window = inject(WA_WINDOW)

  private readonly lanSubnet = computed(() => {
    const profiles = this.service.data()
    const lan = profiles?.find(p => p.owns_lan)
    if (lan) {
      const [first, second] = lan.gateway_ip.split('.').map(Number)
      return { firstOctet: first ?? 192, secondOctet: second ?? 168 }
    }
    return { firstOctet: 192, secondOctet: 168 }
  })

  protected readonly tableData = computed(() => {
    const profiles = this.service.data()
    const vpns = this.outboundService.data()
    if (!profiles) return []

    return profiles.map(p => ({
      ...p,
      dnsDisplay: this.getDnsDisplay(p),
      routingDisplay: this.getRoutingDisplay(p.outbound, vpns || []),
      lanAccessDisplay: this.getLanAccessDisplay(p.lan_access, profiles.length),
      wanAccessDisplay: this.getWanAccessDisplay(p.wan_access),
    }))
  })

  private getDnsDisplay(profile: SecurityProfile): string {
    switch (profile.dns_source) {
      case 'vpn':
        return 'VPN'
      case 'custom':
        return 'Custom'
      default:
        return 'System'
    }
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

  private getLanAccessDisplay(
    access: SecurityProfile['lan_access'],
    profileCount: number,
  ): string {
    if (access === 'ALL') return 'All'
    if (access === 'SAME_PROFILE') {
      return profileCount <= 1 ? 'All' : 'Same profile'
    }

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
      .subscribe(async result => {
        if (profile) {
          const params = { ...profile, ...result }
          let adminIpChanged = false
          try {
            adminIpChanged = await this.service.updateProfile(
              params,
              profile.gateway_ip,
            )
          } catch (e: any) {
            if (!e?.message?.includes('VPN client')) return
            // IP change blocked by VPN peers — offer to force-delete them
            const confirmed = await firstValueFrom(
              this.dialogs.open(TUI_CONFIRM, {
                label: 'Inbound VPN Will Be Deleted',
                data: {
                  content:
                    "Changing this profile's subnet will invalidate all existing VPN client configurations. The inbound VPN server and its peers will be removed and must be re-created.",
                  yes: 'Delete VPN & Continue',
                  no: 'Cancel',
                },
              }),
            ).catch(() => false)
            if (!confirmed) return
            try {
              adminIpChanged = await this.service.updateProfile(
                { ...params, force: true },
                profile.gateway_ip,
              )
            } catch {
              return
            }
          }

          if (adminIpChanged) {
            const newIp = result.gateway_ip
            const currentHost = this.window.location.hostname

            if (currentHost === profile.gateway_ip) {
              this.dialogs
                .open(
                  "Your router's IP address has changed. The UI is now available at the new address.",
                  {
                    label: 'IP Address Changed',
                    dismissible: false,
                    data: 'Open',
                  },
                )
                .subscribe({
                  complete: () => {
                    this.window.location.href = `http://${newIp}`
                  },
                })
            } else {
              await firstValueFrom(
                this.dialogs
                  .open(new PolymorpheusComponent(ReconnectingDialog), {
                    label: 'Reconnecting',
                    closable: false,
                    dismissible: false,
                    data: { message: 'Applying profile settings...' },
                  })
                  .pipe(catchError(() => EMPTY)),
              )
              this.networkRestart.recovered()
              this.alerts
                .open('Profile updated', { appearance: 'positive' })
                .subscribe()
            }
          }
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

export default [
  { path: '', component: Profiles },
  {
    path: ':interface/schedule',
    loadComponent: () => import('./routes/schedule'),
  },
  { path: '**', redirectTo: '' },
] satisfies Routes
