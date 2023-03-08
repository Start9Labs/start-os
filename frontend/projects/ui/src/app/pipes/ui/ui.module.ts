import { NgModule } from '@angular/core'
import { UiPipe, UiAddressesPipe, AddressTypePipe } from './ui.pipe'

@NgModule({
  declarations: [UiPipe, UiAddressesPipe, AddressTypePipe],
  exports: [UiPipe, UiAddressesPipe, AddressTypePipe],
})
export class UiPipeModule {}
