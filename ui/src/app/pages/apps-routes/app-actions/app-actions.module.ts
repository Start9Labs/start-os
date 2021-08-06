import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppActionsPage, AppActionsItemComponent } from './app-actions.page'
import { QRComponentModule } from 'src/app/components/qr/qr.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { AppActionInputPageModule } from 'src/app/modals/app-action-input/app-action-input.module'
import { AppRestoreComponentModule } from 'src/app/modals/app-restore/app-restore.component.module'

const routes: Routes = [
  {
    path: '',
    component: AppActionsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    QRComponentModule,
    SharingModule,
    AppActionInputPageModule,
    AppRestoreComponentModule,
  ],
  declarations: [
    AppActionsPage,
    AppActionsItemComponent,
  ],
})
export class AppActionsPageModule { }
