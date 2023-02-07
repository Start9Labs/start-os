import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BadgeMenuComponent } from './badge-menu.component'
import { TuiLetModule } from '@taiga-ui/cdk'

@NgModule({
  imports: [CommonModule, IonicModule, TuiLetModule],
  declarations: [BadgeMenuComponent],
  exports: [BadgeMenuComponent],
})
export class BadgeMenuComponentModule {}
