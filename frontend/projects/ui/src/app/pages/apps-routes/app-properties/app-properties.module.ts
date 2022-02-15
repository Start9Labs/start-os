import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppPropertiesPage } from './app-properties.page'
import { QRComponentModule } from 'src/app/components/qr/qr.component.module'
import { MaskPipeModule } from 'src/app/pipes/mask/mask.module'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'

const routes: Routes = [
  {
    path: '',
    component: AppPropertiesPage,
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
  ],
  declarations: [AppPropertiesPage],
})
export class AppPropertiesPageModule {}
