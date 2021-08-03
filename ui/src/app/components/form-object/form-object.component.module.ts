import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormObjectComponent, FormObjectHeaderComponent } from './form-object.component'
import { IonicModule } from '@ionic/angular'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { SharingModule } from 'src/app/modules/sharing.module'
import { EnumListPageModule } from 'src/app/modals/enum-list/enum-list.module'

@NgModule({
  declarations: [
    FormObjectComponent,
    FormObjectHeaderComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    ReactiveFormsModule,
    SharingModule,
    EnumListPageModule,
  ],
  exports: [
    FormObjectComponent,
    FormObjectHeaderComponent,
  ],
})
export class FormObjectComponentModule { }
