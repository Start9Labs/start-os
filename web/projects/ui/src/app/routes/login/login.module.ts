import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterModule, Routes } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { TuiAutoFocus } from '@taiga-ui/cdk'
import { TuiButton, TuiError, TuiIcon, TuiTextfield } from '@taiga-ui/core'
import { TuiPassword } from '@taiga-ui/kit'
import { TuiCardLarge } from '@taiga-ui/layout'
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
    ...TuiTextfield,
    TuiIcon,
    TuiPassword,
    TuiAutoFocus,
    TuiError,
    RouterModule.forChild(routes),
    i18nPipe,
  ],
  declarations: [LoginPage],
})
export class LoginPageModule {}
