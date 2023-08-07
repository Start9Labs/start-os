import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { LaunchMenuComponent } from './launch-menu.component'

@NgModule({
  declarations: [LaunchMenuComponent],
  imports: [CommonModule, IonicModule],
  exports: [LaunchMenuComponent],
})
export class LaunchMenuComponentModule {}
