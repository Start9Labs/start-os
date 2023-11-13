import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { WifiPage } from './wifi.page'
import { SharedPipesModule } from '@start9labs/shared'

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
  ],
  declarations: [WifiPage],
})
export class WifiPageModule {}
