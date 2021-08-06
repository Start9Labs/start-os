import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ServerMetricsPage } from './server-metrics.page'
import { SkeletonListComponentModule } from 'src/app/components/skeleton-list/skeleton-list.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'

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
    SkeletonListComponentModule,
    SharingModule,
  ],
  declarations: [ServerMetricsPage],
})
export class ServerMetricsPageModule { }
