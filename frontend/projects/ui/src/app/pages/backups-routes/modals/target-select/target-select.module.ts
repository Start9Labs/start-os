import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { TargetSelectPage, TargetStatusComponent } from './target-select.page'
import { TargetPipesModule } from '../../pipes/target-pipes.module'

@NgModule({
  declarations: [TargetSelectPage, TargetStatusComponent],
  imports: [CommonModule, IonicModule, TargetPipesModule],
  exports: [TargetSelectPage],
})
export class TargetSelectPageModule {}
