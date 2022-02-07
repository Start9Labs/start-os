import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { StatusComponent } from './status.component'
import { SharedPipesModule } from '../../pipes/pipes.module'

@NgModule({
  declarations: [StatusComponent],
  imports: [CommonModule, IonicModule, SharedPipesModule],
  exports: [StatusComponent],
})
export class StatusComponentModule {}
