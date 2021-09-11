import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { GenericInputComponent } from './generic-input.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { FormsModule } from '@angular/forms'

@NgModule({
  declarations: [GenericInputComponent],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [GenericInputComponent],
})
export class GenericInputComponentModule { }
