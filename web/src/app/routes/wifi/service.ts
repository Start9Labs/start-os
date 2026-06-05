import { inject, Injectable } from '@angular/core'
import {
  ApiService,
  WifiConfig,
  WifiPassword,
} from 'src/app/services/api/api.service'
import { FormService } from 'src/app/services/form.service'
import { i18nPipe } from 'src/app/i18n/i18n.pipe'

export type { WifiConfig, WifiPassword }

@Injectable()
export class WifiService extends FormService<WifiConfig> {
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)

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

  // @TODO matt review
  async updatePassword(index: number, update: Partial<WifiPassword>) {
    await this.actions.run(async () => {
      const current = this.data()
      if (!current) return
      const passwords = current.passwords.map((p, i) =>
        i === index ? { ...p, ...update } : p,
      )
      await this.api.wifiSet({ ...current, passwords })
      this.refresh()
    })
  }

  async saveWithRestart(data: WifiConfig): Promise<boolean> {
    return this.actions.run(() => this.store(data), {
      loading: this.i18n.transform('Restarting WiFi...'),
      success: this.i18n.transform('WiFi settings saved'),
      restart: true,
    })
  }

  saveForSsidChange(data: WifiConfig): Promise<boolean> {
    this.networkRestart.suppress()
    return this.api.wifiSet(data).then(
      () => true,
      () => false,
    )
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
