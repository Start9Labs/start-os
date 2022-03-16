import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { KernelLogsPage } from './kernel-logs.page'
import { SharedPipesModule } from '@start9labs/shared'
import { LogsPageModule } from 'src/app/components/logs/logs.module'

const routes: Routes = [
  {
    path: '',
    component: KernelLogsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    LogsPageModule,
  ],
  declarations: [KernelLogsPage],
})
export class KernelLogsPageModule {}
