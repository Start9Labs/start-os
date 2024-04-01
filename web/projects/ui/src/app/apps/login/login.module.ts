import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterModule, Routes } from '@angular/router'
import { TuiErrorModule, TuiTextfieldControllerModule } from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCardModule,
  TuiSurfaceModule,
} from '@taiga-ui/experimental'
import { TuiInputPasswordModule } from '@taiga-ui/kit'
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
    TuiButtonModule,
    TuiCardModule,
    TuiSurfaceModule,
    TuiInputPasswordModule,
    TuiTextfieldControllerModule,
    TuiErrorModule,
    RouterModule.forChild(routes),
  ],
  declarations: [LoginPage],
})
export class LoginPageModule {}
