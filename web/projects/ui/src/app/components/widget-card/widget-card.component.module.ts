import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { WidgetCardComponent } from './widget-card.component'
import { AnyLinkModule } from 'src/app/components/any-link/any-link.component.module'

@NgModule({
  declarations: [WidgetCardComponent],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    AnyLinkModule,
  ],
  exports: [WidgetCardComponent],
})
export class WidgetCardComponentModule {}
