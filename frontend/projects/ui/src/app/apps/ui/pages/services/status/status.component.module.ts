import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { InstallProgressPipeModule } from 'src/app/common/install-progress/install-progress.module'
import { StatusComponent } from './status.component'

@NgModule({
  declarations: [StatusComponent],
  imports: [
    CommonModule,
    IonicModule,
    UnitConversionPipesModule,
    InstallProgressPipeModule,
  ],
  exports: [StatusComponent],
})
export class StatusComponentModule {}
