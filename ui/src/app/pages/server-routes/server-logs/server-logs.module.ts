import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'

import { IonicModule } from '@ionic/angular'

import { ServerLogsPage } from './server-logs.page'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'

const routes: Routes = [
  {
    path: '',
    component: ServerLogsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    BadgeMenuComponentModule,
  ],
  declarations: [ServerLogsPage],
})
export class ServerLogsPageModule { }
