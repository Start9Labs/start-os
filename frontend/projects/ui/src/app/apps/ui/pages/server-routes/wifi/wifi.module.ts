import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { WifiPage, ToWifiIconPipe } from './wifi.page'
import { SharedPipesModule } from '@start9labs/shared'
import { TuiLetModule } from '@taiga-ui/cdk'

const routes: Routes = [
  {
    path: '',
    component: WifiPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    TuiLetModule,
  ],
  declarations: [WifiPage, ToWifiIconPipe],
})
export class WifiPageModule {}
