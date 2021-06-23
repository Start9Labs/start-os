import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppManifestPage } from './app-manifest.page'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { FormsModule } from '@angular/forms'

const routes: Routes = [
  {
    path: '',
    component: AppManifestPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    SharingModule,
  ],
  declarations: [AppManifestPage],
})
export class AppManifestPageModule { }
