import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { ResponsiveColModule } from '@start9labs/shared'
import { WidgetListComponent } from './widget-list.component'
import { AnyLinkModule } from 'src/app/components/any-link/any-link.component.module'
import { WidgetCardComponentModule } from '../widget-card/widget-card.component.module'

@NgModule({
  declarations: [WidgetListComponent],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    AnyLinkModule,
    WidgetCardComponentModule,
    ResponsiveColModule,
  ],
  exports: [WidgetListComponent],
})
export class WidgetListComponentModule {}
