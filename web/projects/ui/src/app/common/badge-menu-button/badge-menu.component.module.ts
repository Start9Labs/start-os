import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BadgeMenuComponent } from './badge-menu.component'

@NgModule({
  imports: [CommonModule, IonicModule],
  declarations: [BadgeMenuComponent],
  exports: [BadgeMenuComponent],
})
export class BadgeMenuComponentModule {}
