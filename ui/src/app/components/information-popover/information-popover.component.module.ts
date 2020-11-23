import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { InformationPopoverComponent } from './information-popover.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    InformationPopoverComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [InformationPopoverComponent],
})
export class InformationPopoverComponentModule { }
