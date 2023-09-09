import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { LoginPage } from './login.page'
import { CAWizardComponent } from './ca-wizard/ca-wizard.component'
import { SharedPipesModule } from '@start9labs/shared'
import { TuiHintModule, TuiTooltipModule } from '@taiga-ui/core'

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
    IonicModule,
    SharedPipesModule,
    RouterModule.forChild(routes),
    TuiTooltipModule,
    TuiHintModule,
  ],
  declarations: [LoginPage, CAWizardComponent],
})
export class LoginPageModule {}
