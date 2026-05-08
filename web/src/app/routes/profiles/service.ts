import { inject, Injectable } from '@angular/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import {
  ApiService,
  ProfileCreateInput,
  ProfileIdOpt,
  ProfileUpdateInput,
  ScheduleWindow,
  SecurityProfile,
} from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'
import { isNetworkError } from 'src/app/services/network-restart.service'
import { pauseFor } from 'src/app/utils/pauseFor'

const RESTART_TIMEOUT_MS = 60_000

@Injectable({ providedIn: 'root' })
export class ProfilesService extends FormService<SecurityProfile[]> {
  private readonly api = inject(ApiService)
  private readonly notifications = inject(TuiNotificationMiddleService)

  async load() {
    // Get list of profile IDs
    const profileIds = await this.api.profilesList()

    // Fetch full profile data for each ID
    const profiles = await Promise.all(
      profileIds.map(id => this.api.profileGet(id)),
    )

    return profiles
  }

  async store(_data: SecurityProfile[]) {
    // Not used - individual methods below handle persistence
  }

  async createProfile(
    params: ProfileCreateInput,
    scheduleWindows: ScheduleWindow[] = [],
  ) {
    await this.actions.run(
      async () => {
        const id = await this.api.profileCreate(params)
        if (scheduleWindows.length) {
          await this.api.profileScheduleSet({
            interface: id.interface,
            windows: scheduleWindows,
          })
        }
        this.refresh()
      },
      {
        loading: 'Creating profile and restarting network...',
        success: 'Profile created',
        restart: true,
      },
    )
  }

  /**
   * @returns true if the admin IP changed (caller handles redirect/reconnect)
   * @throws error with 'VPN client' in message when IP change would break VPN peers
   */
  async updateProfile(
    params: ProfileUpdateInput,
    oldGatewayIp?: string,
    scheduleWindows: ScheduleWindow[] = [],
  ): Promise<boolean> {
    const adminIpChanged =
      params.owns_lan && !!oldGatewayIp && oldGatewayIp !== params.gateway_ip

    if (adminIpChanged) {
      // Schedule write doesn't restart the network — land it before the IP
      // change so it survives the redirect to the new admin address.
      await this.api.profileScheduleSet({
        interface: params.interface,
        windows: scheduleWindows,
      })

      const loading = this.notifications
        .open('Applying profile settings...')
        .subscribe()
      this.networkRestart.suppress()
      try {
        await Promise.race([
          this.api.profileUpdate(params),
          pauseFor(RESTART_TIMEOUT_MS).then(() => {
            throw Object.assign(new Error('Network timeout'), { code: 0 })
          }),
        ])
      } catch (e: any) {
        if (isNetworkError(e)) return true
        this.networkRestart.recovered()
        throw e
      } finally {
        loading.unsubscribe()
      }
      return true
    }

    await this.actions.run(
      async () => {
        await this.api.profileUpdate(params)
        await this.api.profileScheduleSet({
          interface: params.interface,
          windows: scheduleWindows,
        })
        this.refresh()
      },
      {
        loading: 'Applying profile settings...',
        success: 'Profile updated',
        restart: true,
      },
    )

    return false
  }

  async deleteProfile(params: ProfileIdOpt) {
    await this.actions.run(
      async () => {
        await this.api.profileDelete(params)
        this.refresh()
      },
      {
        loading: 'Deleting profile and restarting network...',
        success: 'Profile deleted',
        restart: true,
      },
    )
  }
}
