import { inject, Injectable, signal } from '@angular/core'
import { ApiService, SystemInfoRes, VersionInfo } from './api/api.service'

@Injectable({
  providedIn: 'root',
})
export class SystemService {
  private readonly api = inject(ApiService)

  readonly info = signal<SystemInfoRes | null>(null)
  readonly newerVersions = signal<VersionInfo[]>([])
  readonly updateAvailable = signal(false)

  async init(): Promise<void> {
    try {
      const [info, newerVersions] = await Promise.all([
        this.api.systemInfo(),
        this.api.systemNewerVersions(),
      ])

      this.info.set(info)
      this.newerVersions.set(newerVersions)
      this.updateAvailable.set(newerVersions.length > 0)
    } catch (e) {
      console.error('Failed to fetch system info:', e)
    }
  }
}
