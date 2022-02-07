import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'
import { BadgeMenuComponent } from './badge-menu.component'

@NgModule({
  declarations: [BadgeMenuComponent],
  imports: [CommonModule, IonicModule, SharedPipesModule],
  exports: [BadgeMenuComponent],
})
export class BadgeMenuComponentModule {}
