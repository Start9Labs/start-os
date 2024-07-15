import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterModule, Routes } from '@angular/router'
import { TuiButton, TuiError } from '@taiga-ui/core'
import { TuiCardLarge } from '@taiga-ui/layout'
import {
  TuiInputPasswordModule,
  TuiTextfieldControllerModule,
} from '@taiga-ui/legacy'
import { CAWizardComponent } from './ca-wizard/ca-wizard.component'
import { LoginPage } from './login.page'

const routes: Routes = [
  {
    path: '',
    component: LoginPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    CAWizardComponent,
    TuiButton,
    TuiCardLarge,
    TuiInputPasswordModule,
    TuiTextfieldControllerModule,
    TuiError,
    RouterModule.forChild(routes),
  ],
  declarations: [LoginPage],
})
export class LoginPageModule {}
