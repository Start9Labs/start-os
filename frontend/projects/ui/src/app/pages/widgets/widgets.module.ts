import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { TuiLetModule } from '@taiga-ui/cdk'
import { TuiTilesModule } from '@taiga-ui/kit'

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
    TuiLetModule,
    RouterModule.forChild(routes),
  ],
  declarations: [WidgetsPage],
  exports: [WidgetsPage],
})
export class WidgetsPageModule {}
