import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { StatusComponent } from './status.component'
import { UnitConversionPipesModule } from '../../pipes/unit-conversion/unit-conversion.module'

@NgModule({
  declarations: [StatusComponent],
  imports: [CommonModule, IonicModule, UnitConversionPipesModule],
  exports: [StatusComponent],
})
export class StatusComponentModule {}
