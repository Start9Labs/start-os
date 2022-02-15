import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import {
  FormObjectComponent,
  FormLabelComponent,
  FormErrorComponent,
} from './form-object.component'
import { IonicModule } from '@ionic/angular'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { SharedPipesModule } from '@start9labs/shared'
import { EnumListPageModule } from 'src/app/modals/enum-list/enum-list.module'

@NgModule({
  declarations: [FormObjectComponent, FormLabelComponent, FormErrorComponent],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    ReactiveFormsModule,
    SharedPipesModule,
    EnumListPageModule,
  ],
  exports: [FormObjectComponent, FormLabelComponent, FormErrorComponent],
})
export class FormObjectComponentModule {}
