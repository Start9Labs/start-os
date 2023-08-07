import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppInterfacePage } from './app-interface.page'
import { InterfaceAddressesComponentModule } from 'src/app/common/interface-addresses/interface-addresses.module'

const routes: Routes = [
  {
    path: '',
    component: AppInterfacePage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    InterfaceAddressesComponentModule,
  ],
  declarations: [AppInterfacePage],
})
export class AppInterfacePageModule {}
