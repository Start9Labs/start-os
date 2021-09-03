import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupConfirmationComponent } from 'src/app/modals/backup-confirmation/backup-confirmation.component'
import { DiskInfo } from 'src/app/services/api/api.types'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'app-restore',
  templateUrl: './app-restore.component.html',
  styleUrls: ['./app-restore.component.scss'],
})
export class AppRestoreComponent {
  @Input() pkgId: string
  disks: DiskInfo
  loading = true
  submitting = false
  allPartitionsMounted: boolean
  modal: HTMLIonModalElement

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    public readonly patch: PatchDbService,
  ) { }

  async ngOnInit () {
    this.getExternalDisks()
    this.modal = await this.modalCtrl.getTop()
  }

  async refresh () {
    this.loading = true
    await this.getExternalDisks()
  }

  async getExternalDisks (): Promise<void> {
    try {
      this.disks = await this.embassyApi.getDisks({ })
      this.allPartitionsMounted = Object.values(this.disks).every(d => Object.values(d.partitions).every(p => p['is-mounted']))
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  async presentModal (logicalname: string): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Enter Password',
        message: 'Backup encrypted. Enter the password that was originally used to encrypt this backup.',
        label: 'Password',
        useMask: true,
        buttonText: 'Restore',
        submitFn: async (value: string) => await this.restore(logicalname, value),
      },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: BackupConfirmationComponent,
    })

    modal.onWillDismiss().then(res => {
      if (res.role === 'success') this.modal.dismiss(undefined, 'success')
    })

    await modal.present()
  }

  dismiss () {
    this.modalCtrl.dismiss()
  }

  private async restore (logicalname: string, password: string): Promise<void> {
    this.submitting = true
    try {
      await this.embassyApi.restorePackage({
        id: this.pkgId,
        logicalname,
        password,
      })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.submitting = false
    }
  }
}
