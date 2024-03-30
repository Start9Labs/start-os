import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { StatusComponent } from './status.component'
import { InstallingProgressPipeModule } from 'src/app/pipes/install-progress/install-progress.module'

@NgModule({
  declarations: [StatusComponent],
  imports: [
    CommonModule,
    IonicModule,
    UnitConversionPipesModule,
    InstallingProgressPipeModule,
  ],
  exports: [StatusComponent],
})
export class StatusComponentModule {}
