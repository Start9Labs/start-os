import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'
import { QrCodeModule } from 'ng-qrcode'
import {
  AppInterfacesItemComponent,
  AppInterfacesPage,
} from './app-interfaces.page'
import { UiPipesModule } from '../ui-pipes/ui.module'
import { QRComponent } from './qr.component'

const routes: Routes = [
  {
    path: '',
    component: AppInterfacesPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    UiPipesModule,
    QrCodeModule,
  ],
  declarations: [AppInterfacesPage, AppInterfacesItemComponent, QRComponent],
})
export class AppInterfacesPageModule {}
