import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppActionsPage, AppActionsItemComponent } from './app-actions.page'
import { QRComponentModule } from 'src/app/components/qr/qr.component.module'
import { SharedPipesModule } from '@start9labs/shared'

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
  ],
  declarations: [AppActionsPage, AppActionsItemComponent],
})
export class AppActionsPageModule {}
