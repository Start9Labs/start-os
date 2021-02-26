import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { ExternalDrivesPage } from './external-drives.page'
import { Routes, RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { ObjectConfigComponentModule } from 'src/app/components/object-config/object-config.component.module'
// TODO: EJECT-DISKS

const routes: Routes = [
  {
    path: '',
    component: ExternalDrivesPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
    ObjectConfigComponentModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
  ],
  declarations: [ExternalDrivesPage],
})
export class ExternalDrivesPageModule { }
