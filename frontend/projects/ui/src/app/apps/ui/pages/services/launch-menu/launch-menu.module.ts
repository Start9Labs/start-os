import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { UiPipesModule } from '../ui-pipes/ui.module'
import { LaunchMenuComponent } from './launch-menu.component'

@NgModule({
  declarations: [LaunchMenuComponent],
  imports: [CommonModule, IonicModule, UiPipesModule],
  exports: [LaunchMenuComponent],
})
export class LaunchMenuComponentModule {}
