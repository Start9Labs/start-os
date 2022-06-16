import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { ServerShowPage } from './server-show.page'
import { FormsModule } from '@angular/forms'
import { TextSpinnerComponentModule } from '@start9labs/shared'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { OSUpdatePageModule } from 'src/app/modals/os-update/os-update.page.module'

const routes: Routes = [
  {
    path: '',
    component: ServerShowPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    RouterModule.forChild(routes),
    TextSpinnerComponentModule,
    BadgeMenuComponentModule,
    OSUpdatePageModule,
  ],
  declarations: [ServerShowPage],
})
export class ServerShowPageModule {}
