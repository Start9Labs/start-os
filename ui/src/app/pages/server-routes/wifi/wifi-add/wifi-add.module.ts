import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { WifiAddPage } from './wifi-add.page'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'

const routes: Routes = [
  {
    path: '',
    component: WifiAddPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
  ],
  declarations: [WifiAddPage],
})
export class WifiAddPageModule { }
