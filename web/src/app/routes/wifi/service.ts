import { inject, Injectable } from '@angular/core'
import {
  ApiService,
  WifiConfig,
  WifiPassword,
} from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'

export type { WifiConfig, WifiPassword }

@Injectable()
export class WifiService extends FormService<WifiConfig> {
  private readonly api = inject(ApiService)

  async load() {
    return this.api.wifiGet()
  }

  async store(data: WifiConfig) {
    await this.api.wifiSet(data)
  }

  async addPassword(password: WifiPassword) {
    await this.actions.run(async () => {
      const current = this.data()
      if (!current) return
      await this.api.wifiSet({
        ...current,
        passwords: [...current.passwords, password],
      })
      this.refresh()
    })
  }

  async updatePassword(
    index: number,
    update: { label?: string; profile: string | null },
  ) {
    await this.actions.run(async () => {
      const current = this.data()
      if (!current) return
      const passwords = current.passwords.map((p, i) =>
        i === index
          ? {
              ...p,
              ...update,
              profile: update.profile ? current.passwords[i].profile : null,
            }
          : p,
      )
      await this.api.wifiSet({ ...current, passwords })
      this.refresh()
    })
  }

  async deletePassword(index: number) {
    await this.actions.run(async () => {
      const current = this.data()
      if (!current) return
      const passwords = current.passwords.filter((_, i) => i !== index)
      await this.api.wifiSet({ ...current, passwords })
      this.refresh()
    })
  }
}
