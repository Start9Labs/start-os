import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { PreferencesPage } from './preferences.page'
import { Routes, RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'

const routes: Routes = [
  {
    path: '',
    component: PreferencesPage,
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
    PreferencesPage,
  ],
})
export class PreferencesPageModule { }
