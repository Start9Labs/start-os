import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { MarkdownModule } from '@start9labs/shared'

import { AdditionalComponent } from './additional.component'

@NgModule({
  imports: [IonicModule, MarkdownModule],
  declarations: [AdditionalComponent],
  exports: [AdditionalComponent],
})
export class AdditionalModule {}
