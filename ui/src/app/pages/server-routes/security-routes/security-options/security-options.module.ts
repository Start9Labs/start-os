import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SecurityOptionsPage } from './security-options.page'
import { Routes, RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

const routes: Routes = [
  {
    path: '',
    component: SecurityOptionsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharingModule,
  ],
  declarations: [
    SecurityOptionsPage,
  ],
})
export class SecurityOptionsPageModule { }
