import { CommonModule } from '@angular/common'
import { CUSTOM_ELEMENTS_SCHEMA, NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { QrCodeModule } from 'ng-qrcode'
import { PreloaderComponent } from './preloader.component'

@NgModule({
  imports: [CommonModule, IonicModule, QrCodeModule],
  declarations: [PreloaderComponent],
  exports: [PreloaderComponent],
  schemas: [CUSTOM_ELEMENTS_SCHEMA],
})
export class PreloaderModule {}
