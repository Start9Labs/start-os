import { Component, Input, ViewChild } from '@angular/core'
import { IonInput, LoadingController, ModalController } from '@ionic/angular'
import { ApiService, DiskBackupTarget } from 'src/app/services/api/api.service'
import { RPCEncryptedService } from 'src/app/services/rpc-encrypted.service'

@Component({
  selector: 'prod-key-modal',
  templateUrl: 'prod-key-modal.page.html',
  styleUrls: ['prod-key-modal.page.scss'],
})
export class ProdKeyModal {
  @ViewChild('focusInput') elem?: IonInput
  @Input() target!: DiskBackupTarget

  error = ''
  productKey = ''
  unmasked = false

  constructor(
    private readonly modalController: ModalController,
    private readonly apiService: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly encrypted: RPCEncryptedService,
  ) {}

  ngAfterViewInit() {
    setTimeout(() => this.elem?.setFocus(), 400)
  }

  async verifyProductKey() {
    if (!this.productKey || !this.target.logicalname) return

    const loader = await this.loadingCtrl.create({
      message: 'Verifying Product Key',
    })
    await loader.present()

    try {
      await this.apiService.set02XDrive(this.target.logicalname)
      this.encrypted.productKey = this.productKey
      await this.apiService.verifyProductKey()
      this.modalController.dismiss({ productKey: this.productKey }, 'success')
    } catch (e) {
      this.encrypted.productKey = undefined
      this.error = 'Invalid Product Key'
    } finally {
      loader.dismiss()
    }
  }

  cancel() {
    this.modalController.dismiss()
  }
}
