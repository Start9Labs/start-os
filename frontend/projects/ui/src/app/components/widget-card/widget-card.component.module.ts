import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { WidgetCardComponent } from './widget-card.component'

@NgModule({
  declarations: [WidgetCardComponent],
  imports: [CommonModule, IonicModule, RouterModule.forChild([])],
  exports: [WidgetCardComponent],
})
export class WidgetCardComponentModule {}
