import { Component, Input, ViewChild } from '@angular/core'
import { IonInput, LoadingController, ModalController } from '@ionic/angular'
import { ApiService, DiskBackupTarget } from 'src/app/services/api/api.service'
import { HttpService } from 'src/app/services/api/http.service'

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
    private readonly httpService: HttpService,
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
      this.httpService.productKey = this.productKey
      await this.apiService.verifyProductKey()
      this.modalController.dismiss({ productKey: this.productKey }, 'success')
    } catch (e) {
      this.httpService.productKey = undefined
      this.error = 'Invalid Product Key'
    } finally {
      loader.dismiss()
    }
  }

  cancel() {
    this.modalController.dismiss()
  }
}
