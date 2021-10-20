import { Injectable } from '@angular/core'
import { IonicSafeString } from '@ionic/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getErrorMessage } from 'src/app/services/error-toast.service'
import { MappedDriveInfo, MappedPartitionInfo } from 'src/app/util/misc.util'
import { Emver } from 'src/app/services/emver.service'

@Injectable({
  providedIn: 'root',
})
export class BackupService {
  drives: MappedDriveInfo[]
  loading = true
  loadingError: string | IonicSafeString

  constructor (
    private readonly embassyApi: ApiService,
    private readonly emver: Emver,
  ) { }

  async getExternalDrives (): Promise<void> {
    this.loading = true

    try {
      const drives = await this.embassyApi.getDrives({ })
      this.drives = drives.map(d => {
        const partionInfo: MappedPartitionInfo[] = d.partitions.map(p => {
          return {
            ...p,
            hasBackup: [0, 1].includes(this.emver.compare(p['embassy-os']?.version, '0.3.0')),
          }
        })
        return {
          ...d,
          partitions: partionInfo,
        }
      })
    } catch (e) {
      this.loadingError = getErrorMessage(e)
    } finally {
      this.loading = false
    }
  }

}