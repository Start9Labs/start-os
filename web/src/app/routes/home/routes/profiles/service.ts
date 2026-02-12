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
    await this.actions.run(async () => {
      await this.api.profileCreate(params)
      this.refresh()
    })
  }

  async updateProfile(params: ProfileUpdateInput) {
    await this.actions.run(async () => {
      await this.api.profileUpdate(params)
      this.refresh()
    })
  }

  async deleteProfile(params: ProfileIdOpt) {
    await this.actions.run(async () => {
      await this.api.profileDelete(params)
      this.refresh()
    })
  }
}
