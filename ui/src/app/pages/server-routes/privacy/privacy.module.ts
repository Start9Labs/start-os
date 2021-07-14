import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { PrivacyPage } from './privacy.page'
import { Routes, RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'

const routes: Routes = [
  {
    path: '',
    component: PrivacyPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
  ],
  declarations: [
    PrivacyPage,
  ],
})
export class PrivacyPageModule { }
