import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GenericInputComponent } from 'src/app/modals/generic-input/generic-input.component'
import { DiskInfo } from 'src/app/services/api/api.types'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'server-backup',
  templateUrl: './server-backup.page.html',
  styleUrls: ['./server-backup.page.scss'],
})
export class ServerBackupPage {
  disks: DiskInfo[]
  loading = true
  allPartitionsMounted: boolean

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
  ) { }

  ngOnInit () {
    this.getExternalDisks()
  }

  async doRefresh () {
    this.loading = true
    await this.getExternalDisks()
  }

  async getExternalDisks (): Promise<void> {
    try {
      this.disks = await this.embassyApi.getDisks({ })
    } catch (e) {
      this.errToast.present(e)
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
        useMask: true,
        buttonText: 'Create Backup',
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
