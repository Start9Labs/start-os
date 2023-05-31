import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppCredentialsPage } from './app-credentials.page'
import { QRComponentModule } from 'src/app/components/qr/qr.component.module'
import { MaskPipeModule } from 'src/app/pipes/mask/mask.module'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import { SkeletonListComponentModule } from 'src/app/components/skeleton-list/skeleton-list.component.module'

const routes: Routes = [
  {
    path: '',
    component: AppCredentialsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    QRComponentModule,
    SharedPipesModule,
    TextSpinnerComponentModule,
    MaskPipeModule,
    SkeletonListComponentModule,
  ],
  declarations: [AppCredentialsPage],
})
export class AppCredentialsPageModule {}
