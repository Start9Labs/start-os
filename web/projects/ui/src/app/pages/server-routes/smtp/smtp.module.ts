import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { TuiInputModule } from '@taiga-ui/kit'
import {
  TuiNotificationModule,
  TuiTextfieldControllerModule,
} from '@taiga-ui/core'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { SMTPPage } from './smtp.page'
import { FormModule } from 'src/app/components/form/form.module'
import { IonicModule } from '@ionic/angular'
import { TuiErrorModule, TuiModeModule } from '@taiga-ui/core'
import { TuiAppearanceModule, TuiButtonModule } from '@taiga-ui/experimental'

const routes: Routes = [
  {
    path: '',
    component: SMTPPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    TuiButtonModule,
    TuiInputModule,
    FormModule,
    TuiNotificationModule,
    TuiTextfieldControllerModule,
    TuiAppearanceModule,
    TuiModeModule,
    TuiErrorModule,
  ],
  declarations: [SMTPPage],
})
export class SMTPPageModule {}
