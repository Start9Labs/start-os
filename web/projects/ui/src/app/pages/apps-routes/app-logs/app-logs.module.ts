import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppLogsPage } from './app-logs.page'
import { LogsComponentModule } from 'src/app/components/logs/logs.component.module'

const routes: Routes = [
  {
    path: '',
    component: AppLogsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    LogsComponentModule,
  ],
  declarations: [AppLogsPage],
})
export class AppLogsPageModule {}
