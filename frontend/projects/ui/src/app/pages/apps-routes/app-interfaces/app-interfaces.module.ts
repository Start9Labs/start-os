import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'

import {
  AppInterfacesItemComponent,
  AppInterfacesPage,
} from './app-interfaces.page'

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
  ],
  declarations: [AppInterfacesPage, AppInterfacesItemComponent],
})
export class AppInterfacesPageModule {}
