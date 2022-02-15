import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { StatusComponent } from './status.component'

@NgModule({
  declarations: [StatusComponent],
  imports: [CommonModule, IonicModule, UnitConversionPipesModule],
  exports: [StatusComponent],
})
export class StatusComponentModule {}
