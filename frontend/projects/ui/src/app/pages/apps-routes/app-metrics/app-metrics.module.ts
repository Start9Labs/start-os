import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppMetricsPage } from './app-metrics.page'
import { SharedPipesModule } from '@start9labs/shared'
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
    SharedPipesModule,
    SkeletonListComponentModule,
  ],
  declarations: [AppMetricsPage],
})
export class AppMetricsPageModule {}
