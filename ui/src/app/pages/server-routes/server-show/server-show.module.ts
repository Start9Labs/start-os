import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { ServerShowPage } from './server-show.page'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { FormsModule } from '@angular/forms'
import { SharingModule } from 'src/app/modules/sharing.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'

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
    StatusComponentModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharingModule,
    BadgeMenuComponentModule,
  ],
  declarations: [ServerShowPage],
})
export class ServerShowPageModule { }
