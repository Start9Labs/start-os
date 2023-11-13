import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'

import { FooterComponent } from './footer.component'

@NgModule({
  imports: [CommonModule, IonicModule],
  declarations: [FooterComponent],
  exports: [FooterComponent],
})
export class FooterModule {}
