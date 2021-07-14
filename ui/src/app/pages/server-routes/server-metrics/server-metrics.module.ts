import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ServerMetricsPage } from './server-metrics.page'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { SkeletonListComponentModule } from 'src/app/components/skeleton-list/skeleton-list.component.module'

const routes: Routes = [
  {
    path: '',
    component: ServerMetricsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    SkeletonListComponentModule,
  ],
  declarations: [ServerMetricsPage],
})
export class ServerMetricsPageModule { }
