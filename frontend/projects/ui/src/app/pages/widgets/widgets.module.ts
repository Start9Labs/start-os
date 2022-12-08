import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { TuiArcChartModule, TuiRingChartModule } from '@taiga-ui/addon-charts'
import { TuiProgressModule, TuiTilesModule } from '@taiga-ui/kit'

import { WidgetsPage } from './widgets.page'

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
    TuiRingChartModule,
    TuiArcChartModule,
    TuiProgressModule,
    RouterModule.forChild(routes),
  ],
  declarations: [WidgetsPage],
  exports: [WidgetsPage],
})
export class WidgetsPageModule {}
