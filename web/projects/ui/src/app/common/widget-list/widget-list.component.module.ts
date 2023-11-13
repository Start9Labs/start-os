import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { ResponsiveColModule } from '@start9labs/shared'
import { AnyLinkComponent } from './any-link/any-link.component'
import { WidgetListComponent } from './widget-list.component'
import { WidgetCardComponent } from './widget-card/widget-card.component'

@NgModule({
  declarations: [WidgetListComponent, WidgetCardComponent, AnyLinkComponent],
  imports: [CommonModule, IonicModule, RouterModule, ResponsiveColModule],
  exports: [WidgetListComponent],
})
export class WidgetListComponentModule {}
