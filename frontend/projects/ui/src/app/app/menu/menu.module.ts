import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'

import { MenuComponent } from './menu.component'
import { SnekModule } from '../snek/snek.module'

@NgModule({
  imports: [CommonModule, IonicModule, RouterModule, SnekModule],
  declarations: [MenuComponent],
  exports: [MenuComponent],
})
export class MenuModule {}
