import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SecurityOptionsPage } from './security-options.page'
import { Routes, RouterModule } from '@angular/router'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { ObjectConfigComponentModule } from 'src/app/components/object-config/object-config.component.module'
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
    ObjectConfigComponentModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    SharingModule,
  ],
  declarations: [
    SecurityOptionsPage,
  ],
})
export class SecurityOptionsPageModule { }
