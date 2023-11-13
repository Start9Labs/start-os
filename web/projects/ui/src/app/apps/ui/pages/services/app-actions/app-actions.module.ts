import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'
import { FormPageModule } from 'src/app/apps/ui/modals/form/form.module'
import { ActionSuccessPageModule } from './action-success/action-success.module'
import {
  AppActionsPage,
  AppActionsItemComponent,
  GroupActionsPipe,
} from './app-actions.page'

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
    SharedPipesModule,
    ActionSuccessPageModule,
    FormPageModule,
    RouterModule.forChild(routes),
  ],
  declarations: [AppActionsPage, AppActionsItemComponent, GroupActionsPipe],
})
export class AppActionsPageModule {}
