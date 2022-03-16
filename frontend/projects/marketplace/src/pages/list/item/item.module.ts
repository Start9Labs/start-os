import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'

import { ItemComponent } from './item.component'

@NgModule({
  imports: [IonicModule, RouterModule, SharedPipesModule],
  declarations: [ItemComponent],
  exports: [ItemComponent],
})
export class ItemModule {}
