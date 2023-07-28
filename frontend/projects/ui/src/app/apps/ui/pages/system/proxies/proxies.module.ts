import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterModule, Routes } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  TuiButtonModule,
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiNotificationModule,
  TuiSvgModule,
  TuiWrapperModule,
} from '@taiga-ui/core'
import { TuiBadgeModule, TuiInputModule, TuiToggleModule } from '@taiga-ui/kit'
import { ProxiesPage } from './proxies.page'

const routes: Routes = [
  {
    path: '',
    component: ProxiesPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    FormsModule,
    TuiNotificationModule,
    TuiButtonModule,
    TuiInputModule,
    TuiToggleModule,
    TuiWrapperModule,
    TuiBadgeModule,
    TuiSvgModule,
    TuiHostedDropdownModule,
    TuiDataListModule,
  ],
  declarations: [ProxiesPage],
})
export class ProxiesPageModule {}
