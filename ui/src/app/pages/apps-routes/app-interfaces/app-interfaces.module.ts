import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppInterfacesItemComponent, AppInterfacesPage } from './app-interfaces.page'
import { SharingModule } from 'src/app/modules/sharing.module'

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
    SharingModule,
  ],
  declarations: [
    AppInterfacesPage,
    AppInterfacesItemComponent,
  ],
})
export class AppInterfacesPageModule { }
