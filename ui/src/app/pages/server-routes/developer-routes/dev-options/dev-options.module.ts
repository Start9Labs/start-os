import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { DevOptionsPage } from './dev-options.page'
import { Routes, RouterModule } from '@angular/router'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { ObjectConfigComponentModule } from 'src/app/components/object-config/object-config.component.module'

const routes: Routes = [
  {
    path: '',
    component: DevOptionsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    ObjectConfigComponentModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    BadgeMenuComponentModule,
  ],
  declarations: [DevOptionsPage],
})
export class DevOptionsPageModule { }
