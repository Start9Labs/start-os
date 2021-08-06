import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { WifiListPage } from './wifi.page'
import { SharingModule } from 'src/app/modules/sharing.module'

const routes: Routes = [
  {
    path: '',
    component: WifiListPage,
  },
  {
    path: 'add',
    loadChildren: () => import('./wifi-add/wifi-add.module').then(m => m.WifiAddPageModule),
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharingModule,
  ],
  declarations: [WifiListPage],
})
export class WifiListPageModule { }
