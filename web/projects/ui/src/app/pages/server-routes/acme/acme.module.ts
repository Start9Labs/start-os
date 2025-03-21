import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { ACMEPage } from './acme.page'

const routes: Routes = [
  {
    path: '',
    component: ACMEPage,
  },
]

@NgModule({
  imports: [CommonModule, IonicModule, RouterModule.forChild(routes)],
  declarations: [ACMEPage],
})
export class ACMEPageModule {}
