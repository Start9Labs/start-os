import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'

import { ItemComponent } from './item.component'

@NgModule({
  imports: [CommonModule, IonicModule, RouterModule, SharedPipesModule],
  declarations: [ItemComponent],
  exports: [ItemComponent],
})
export class ItemModule {}
