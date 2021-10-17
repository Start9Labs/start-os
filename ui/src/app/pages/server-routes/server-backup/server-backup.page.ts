import { Component } from '@angular/core'
import { ModalController, IonicSafeString } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GenericInputComponent } from 'src/app/modals/generic-input/generic-input.component'
import { getErrorMessage } from 'src/app/services/error-toast.service'
import { MappedDiskInfo, MappedPartitionInfo } from 'src/app/util/misc.util'
import { Emver } from 'src/app/services/emver.service'

@Component({
  selector: 'server-backup',
  templateUrl: './server-backup.page.html',
  styleUrls: ['./server-backup.page.scss'],
})
export class ServerBackupPage {
  disks: MappedDiskInfo[]
  loading = true
  loadingError: string | IonicSafeString

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly emver: Emver,
    private readonly embassyApi: ApiService,
  ) { }

  ngOnInit () {
    this.getExternalDisks()
  }

  async refresh () {
    this.loading = true
    await this.getExternalDisks()
  }

  async getExternalDisks (): Promise<void> {
    try {
      const disks = await this.embassyApi.getDisks({ })
      this.disks = disks.map(d => {
        const partionInfo: MappedPartitionInfo[] = d.partitions.map(p => {
          return {
            ...p,
            hasBackup: [0, 1].includes(this.emver.compare(p['embassy-os']?.version, '0.3.0')),
            backupInfo: null,
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

  async presentModal (logicalname: string): Promise<void> {
    const m = await this.modalCtrl.create({
      componentProps: {
        title: 'Create Backup',
        message: `Enter your master password to create an encrypted backup of your Embassy and all its installed services.`,
        label: 'Password',
        placeholder: 'Enter password',
        useMask: true,
        buttonText: 'Create Backup',
        loadingText: 'Beginning backup...',
        submitFn: async (value: string) => await this.create(logicalname, value),
      },
      cssClass: 'alertlike-modal',
      component: GenericInputComponent,
    })

    return await m.present()
  }

  private async create (logicalname: string, password: string): Promise<void> {
    await this.embassyApi.createBackup({ logicalname, password })
  }
}
