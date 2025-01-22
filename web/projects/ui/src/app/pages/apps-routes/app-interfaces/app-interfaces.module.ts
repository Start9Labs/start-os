import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'
import { AppInterfacesPage } from './app-interfaces.page'
import { InterfaceInfoModule } from 'src/app/components/interface-info/interface-info.module'

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
    InterfaceInfoModule,
  ],
  declarations: [AppInterfacesPage],
})
export class AppInterfacesPageModule {}
