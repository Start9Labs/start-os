import { Component, Input } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ConfigCursor } from 'src/app/pkg-config/config-cursor'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { Action } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-action-input',
  templateUrl: './app-action-input.page.html',
  styleUrls: ['./app-action-input.page.scss'],
})
export class AppActionInputPage {
  @Input() action: Action
  @Input() cursor: ConfigCursor<'object'>
  @Input() execute: () => Promise<void>
  spec: ValueSpecObject
  value: object
  error: string

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
  ) { }

  ngOnInit () {
    this.spec = this.cursor.spec()
    this.value = this.cursor.config()
    this.error = this.cursor.checkInvalid()
  }

  async dismiss (): Promise<void> {
    this.modalCtrl.dismiss()
  }

  async save (): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Executing action',
      cssClass: 'loader-ontop-of-all',
    })
    await loader.present()

    try {
      await this.execute()
      this.modalCtrl.dismiss()
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  handleObjectEdit (): void {
    this.error = this.cursor.checkInvalid()
  }
}
