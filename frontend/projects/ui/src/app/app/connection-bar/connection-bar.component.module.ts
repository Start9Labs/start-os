import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { ConnectionBarComponent } from './connection-bar.component'

@NgModule({
  declarations: [ConnectionBarComponent],
  imports: [CommonModule, IonicModule],
  exports: [ConnectionBarComponent],
})
export class ConnectionBarComponentModule {}
