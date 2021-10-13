import { Component, Input } from '@angular/core'
import { ModalController, IonicSafeString } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GenericInputComponent } from 'src/app/modals/generic-input/generic-input.component'
import { DiskInfo } from 'src/app/services/api/api.types'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { getErrorMessage } from 'src/app/services/error-toast.service'

@Component({
  selector: 'app-restore',
  templateUrl: './app-restore.component.html',
  styleUrls: ['./app-restore.component.scss'],
})
export class AppRestoreComponent {
  @Input() pkgId: string
  disks: DiskInfo[]
  loading = true
  allPartitionsMounted: boolean
  modal: HTMLIonModalElement
  loadingError: string | IonicSafeString

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
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
    } catch (e) {
      this.loadingError = getErrorMessage(e)
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
        submitFn: (value: string) => this.restore(logicalname, value),
      },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
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
    await this.embassyApi.restorePackage({
      id: this.pkgId,
      logicalname,
      password,
    })
  }
}
