import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { RouterModule, Routes } from '@angular/router'
import { TuiButtonModule } from '@taiga-ui/core'
import { HomePage } from './home.page'

const ROUTES: Routes = [
  {
    path: '',
    component: HomePage,
  },
]

@NgModule({
  imports: [CommonModule, TuiButtonModule, RouterModule.forChild(ROUTES)],
  declarations: [HomePage],
})
export class HomePageModule {}
