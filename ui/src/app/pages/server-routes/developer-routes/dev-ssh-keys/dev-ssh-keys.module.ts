import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { DevSSHKeysPage } from './dev-ssh-keys.page'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'

const routes: Routes = [
  {
    path: '',
    component: DevSSHKeysPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    BadgeMenuComponentModule,
  ],
  declarations: [DevSSHKeysPage],
})
export class DevSSHKeysPageModule { }
