import { TuiButton } from '@taiga-ui/core'
import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { RouterModule, Routes } from '@angular/router'
import { HomePage } from './home.page'

const ROUTES: Routes = [
  {
    path: '',
    component: HomePage,
  },
]

@NgModule({
  imports: [CommonModule, TuiButton, RouterModule.forChild(ROUTES)],
  declarations: [HomePage],
})
export class HomePageModule {}
