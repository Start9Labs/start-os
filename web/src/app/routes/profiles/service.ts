import { inject, Injectable } from '@angular/core'
import {
  ApiService,
  ProfileCreateInput,
  ProfileIdOpt,
  ProfileUpdateInput,
  SecurityProfile,
} from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'

@Injectable({ providedIn: 'root' })
export class ProfilesService extends FormService<SecurityProfile[]> {
  private readonly api = inject(ApiService)

  async load() {
    // Get list of profile IDs
    const profileIds = await this.api.profilesList()

    // Fetch full profile data for each ID
    const profiles = await Promise.all(
      profileIds.map(id => this.api.profileGet(id)),
    )

    return profiles
  }

  async store(data: SecurityProfile[]) {
    // Not used - individual methods below handle persistence
  }

  async createProfile(params: ProfileCreateInput) {
    await this.actions.run(
      async () => {
        await this.api.profileCreate(params)
        this.refresh()
      },
      {
        loading: 'Creating profile and restarting network...',
        success: 'Profile created',
        restart: true,
      },
    )
  }

  async updateProfile(
    params: ProfileUpdateInput,
    oldGatewayIp?: string,
  ): Promise<boolean> {
    const adminIpChanged =
      params.owns_lan && !!oldGatewayIp && oldGatewayIp !== params.gateway_ip

    if (adminIpChanged) {
      this.networkRestart.suppress()
      await this.api.profileUpdate(params).catch(() => {})
      return true
    }

    await this.actions.run(
      async () => {
        await this.api.profileUpdate(params)
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
