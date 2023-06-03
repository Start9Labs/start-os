import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { TargetSelectPage, TargetStatusComponent } from './target-select.page'
import { TargetPipesModule } from '../../pipes/target-pipes.module'
import { TextSpinnerComponentModule } from '@start9labs/shared'

@NgModule({
  declarations: [TargetSelectPage, TargetStatusComponent],
  imports: [
    CommonModule,
    IonicModule,
    TargetPipesModule,
    TextSpinnerComponentModule,
  ],
  exports: [TargetSelectPage],
})
export class TargetSelectPageModule {}
