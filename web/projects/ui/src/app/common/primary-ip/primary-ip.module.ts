import { NgModule } from '@angular/core'
import { PrimaryIpPipe } from './primary-ip.pipe'

@NgModule({
  declarations: [PrimaryIpPipe],
  exports: [PrimaryIpPipe],
})
export class PrimaryIpPipeModule {}
