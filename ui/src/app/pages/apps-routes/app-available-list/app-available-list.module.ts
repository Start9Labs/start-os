import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppAvailableListPage } from './app-available-list.page'
import { SharingModule } from '../../../modules/sharing.module'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'


const routes: Routes = [
  {
    path: '',
    component: AppAvailableListPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    StatusComponentModule,
    SharingModule,
    PwaBackComponentModule,
    BadgeMenuComponentModule,
  ],
  declarations: [AppAvailableListPage],
})
export class AppAvailableListPageModule { }
