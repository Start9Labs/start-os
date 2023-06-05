import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { PortForwardsPage } from './port-forwards.page'
import { PrimaryIpPipeModule } from 'src/app/common/primary-ip/primary-ip.module'

const routes: Routes = [
  {
    path: '',
    component: PortForwardsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    PrimaryIpPipeModule,
  ],
  declarations: [PortForwardsPage],
})
export class PortForwardsPageModule {}
