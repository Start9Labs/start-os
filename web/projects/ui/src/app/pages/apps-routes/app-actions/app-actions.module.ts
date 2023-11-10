import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppActionsPage, AppActionsItemComponent } from './app-actions.page'
import { QRComponentModule } from 'src/app/components/qr/qr.component.module'
import { SharedPipesModule } from '@start9labs/shared'
import { GenericFormPageModule } from 'src/app/modals/generic-form/generic-form.module'
import { ActionSuccessPageModule } from 'src/app/modals/action-success/action-success.module'

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
    SharedPipesModule,
    GenericFormPageModule,
    ActionSuccessPageModule,
  ],
  declarations: [AppActionsPage, AppActionsItemComponent],
})
export class AppActionsPageModule {}
