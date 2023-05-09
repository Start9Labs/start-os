import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { LaunchMenuComponent } from './launch-menu.component'
import { UiPipeModule } from 'src/app/pipes/ui/ui.module'

@NgModule({
  declarations: [LaunchMenuComponent],
  imports: [CommonModule, IonicModule, UiPipeModule],
  exports: [LaunchMenuComponent],
})
export class LaunchMenuComponentModule {}
