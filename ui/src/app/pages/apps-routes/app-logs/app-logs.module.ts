import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppLogsPage } from './app-logs.page'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'

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
    PwaBackComponentModule,
  ],
  declarations: [AppLogsPage],
})
export class AppLogsPageModule { }
