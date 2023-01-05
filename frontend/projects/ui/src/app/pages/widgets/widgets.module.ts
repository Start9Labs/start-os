import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { TuiLetModule } from '@taiga-ui/cdk'
import { TuiLoaderModule } from '@taiga-ui/core'
import { TuiTilesModule } from '@taiga-ui/kit'

import { WidgetsPage } from './widgets.page'
import { AddWidgetModule } from './built-in/add/add.module'
import { FavoritesModule } from './built-in/favorites/favorites.module'
import { HealthModule } from './built-in/health/health.module'
import { MetricsModule } from './built-in/metrics/metrics.module'
import { NetworkModule } from './built-in/network/network.module'
import { UptimeModule } from './built-in/uptime/uptime.module'

const routes: Routes = [
  {
    path: '',
    component: WidgetsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    TuiTilesModule,
    TuiLetModule,
    AddWidgetModule,
    FavoritesModule,
    HealthModule,
    MetricsModule,
    NetworkModule,
    UptimeModule,
    RouterModule.forChild(routes),
    TuiLoaderModule,
  ],
  declarations: [WidgetsPage],
  exports: [WidgetsPage],
})
export class WidgetsPageModule {}
