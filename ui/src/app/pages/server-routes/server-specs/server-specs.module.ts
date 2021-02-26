import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ServerSpecsPage } from './server-specs.page'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'

const routes: Routes = [
  {
    path: '',
    component: ServerSpecsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    SharingModule,
  ],
  declarations: [ServerSpecsPage],
})
export class ServerSpecsPageModule { }
