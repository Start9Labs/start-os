import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { OSAddressesPage } from './os-addresses.page'

const routes: Routes = [
  {
    path: '',
    component: OSAddressesPage,
  },
]

@NgModule({
  imports: [CommonModule, IonicModule, RouterModule.forChild(routes)],
  declarations: [OSAddressesPage],
})
export class OSAddressesPageModule {}
