import { inject, Injectable, signal } from '@angular/core'
import { ErrorService, getErrorMessage } from '@start9labs/shared'
import { Version } from '@start9labs/start-sdk'
import {
  BackupTarget,
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

export interface MappedBackupTarget<T> {
  id: string
  hasAnyBackup: boolean
  entry: T
}

@Injectable({
  providedIn: 'root',
})
export class BackupService {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)

  readonly cifs = signal<MappedBackupTarget<CifsBackupTarget>[]>([])
  readonly drives = signal<MappedBackupTarget<DiskBackupTarget>[]>([])
  readonly loading = signal(true)

  async getBackupTargets(): Promise<void> {
    this.loading.set(true)

    try {
      const targets = await this.api.getBackupTargets({})

      this.cifs.set(
        Object.entries(targets)
          .filter(([_, target]) => target.type === 'cifs')
          .map(([id, cifs]) => {
            return {
              id,
              hasAnyBackup: this.hasAnyBackup(cifs),
              entry: cifs as CifsBackupTarget,
            }
          }),
      )

      this.drives.set(
        Object.entries(targets)
          .filter(([_, target]) => target.type === 'disk')
          .map(([id, drive]) => {
            return {
              id,
              hasAnyBackup: this.hasAnyBackup(drive),
              entry: drive as DiskBackupTarget,
            }
          }),
      )
    } catch (e: any) {
      this.errorService.handleError(getErrorMessage(e))
    } finally {
      this.loading.set(false)
    }
  }

  hasAnyBackup({ startOs }: BackupTarget): boolean {
    return Object.values(startOs).some(
      s => Version.parse(s.version).compare(Version.parse('0.3.6')) !== 'less',
    )
  }

  hasThisBackup({ startOs }: BackupTarget, id: string): boolean {
    return (
      startOs[id] &&
      Version.parse(startOs[id].version).compare(Version.parse('0.3.6')) !==
        'less'
    )
  }
}
