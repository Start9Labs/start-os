import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppInstructionsPage } from './app-instructions.page'
import { SharingModule } from 'src/app/modules/sharing.module'

const routes: Routes = [
  {
    path: '',
    component: AppInstructionsPage,
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
    AppInstructionsPage,
  ],
})
export class AppInstructionsPageModule { }
