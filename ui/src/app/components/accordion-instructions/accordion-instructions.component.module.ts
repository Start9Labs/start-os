import { NgModule }           from '@angular/core'
import { CommonModule }       from '@angular/common'
import { SharingModule }                  from '../../modules/sharing.module'
import { AccordionInstructionsComponent } from './accordion-instructions.component'
import { IonicModule }                    from '@ionic/angular'

@NgModule({
  declarations: [
    AccordionInstructionsComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
  ],
  exports: [AccordionInstructionsComponent],
})
export class AccordionInstructionsComponentModule { }
