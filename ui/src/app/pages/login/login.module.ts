import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { LoginPage } from './login.page'
import { SharingModule } from 'src/app/modules/sharing.module'
// import { MarketplaceLibModule } from 'marketplace-lib'

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
    RouterModule.forChild(routes),
    SharingModule,
    // MarketplaceLibModule,
  ],
  declarations: [LoginPage],
})
export class LoginPageModule { }
