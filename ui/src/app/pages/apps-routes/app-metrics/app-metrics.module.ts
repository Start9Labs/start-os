import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppMetricsPage } from './app-metrics.page'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { SkeletonListComponentModule } from 'src/app/components/skeleton-list/skeleton-list.component.module'

const routes: Routes = [
  {
    path: '',
    component: AppMetricsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    SharingModule,
    SkeletonListComponentModule,
  ],
  declarations: [AppMetricsPage],
})
export class AppMetricsPageModule { }
