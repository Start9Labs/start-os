import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormObjectComponent } from './form-object.component'
import { IonicModule } from '@ionic/angular'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'

@NgModule({
  declarations: [
    FormObjectComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    ReactiveFormsModule,
  ],
  exports: [FormObjectComponent],
})
export class FormObjectComponentModule { }
