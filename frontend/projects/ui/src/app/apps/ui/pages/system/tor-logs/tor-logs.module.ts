import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { TorLogsPage } from './tor-logs.page'
import { LogsComponentModule } from 'src/app/common/logs/logs.component.module'

const routes: Routes = [
  {
    path: '',
    component: TorLogsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    LogsComponentModule,
  ],
  declarations: [TorLogsPage],
})
export class TorLogsPageModule {}
