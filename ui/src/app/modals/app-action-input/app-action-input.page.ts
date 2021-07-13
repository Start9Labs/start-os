import { Component, Input } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ConfigCursor } from 'src/app/pkg-config/config-cursor'
import { ValueSpecObject } from 'src/app/pkg-config/config-types'
import { LoaderService } from 'src/app/services/loader.service'
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
    private readonly loadingCtrl: LoadingController,
    private loaderService: LoaderService,
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
    this.loaderService.of({
      spinner: 'lines',
      message: 'Executing action',
      cssClass: 'loader-ontop-of-all',
    }).displayDuringAsync(async () => {
      try {
        await this.execute()
        this.modalCtrl.dismiss()
      } catch (e) {
        this.error = e.message
      }
    })
  }

  handleObjectEdit (): void {
    this.error = this.cursor.checkInvalid()
  }
}
