import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ServerLogsPage } from './server-logs.page'
import { SharingModule } from 'src/app/modules/sharing.module'
import { LogsPageModule } from 'src/app/components/logs/logs.module'

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
    SharingModule,
    LogsPageModule,
  ],
  declarations: [ServerLogsPage],
})
export class ServerLogsPageModule { }
