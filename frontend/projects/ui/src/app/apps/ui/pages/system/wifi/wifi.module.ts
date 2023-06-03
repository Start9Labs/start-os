import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'
import { TuiLetModule } from '@taiga-ui/cdk'
import { FormPageModule } from 'src/app/apps/ui/modals/form/form.module'
import { WifiPage, ToWifiIconPipe } from './wifi.page'

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
    SharedPipesModule,
    TuiLetModule,
    FormPageModule,
    RouterModule.forChild(routes),
  ],
  declarations: [WifiPage, ToWifiIconPipe],
})
export class WifiPageModule {}
