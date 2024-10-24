import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { HomePage } from './home.page'

const routes: Routes = [
  {
    path: '',
    component: HomePage,
  },
]

@NgModule({
  imports: [CommonModule, IonicModule, RouterModule.forChild(routes)],
  declarations: [HomePage],
})
export class HomePageModule {}
