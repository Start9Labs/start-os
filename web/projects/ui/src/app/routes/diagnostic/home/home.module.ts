import { TuiButton } from '@taiga-ui/core'
import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { RouterModule, Routes } from '@angular/router'
import { HomePage } from './home.page'
import { i18nPipe } from '@start9labs/shared'

const ROUTES: Routes = [
  {
    path: '',
    component: HomePage,
  },
]

@NgModule({
  imports: [CommonModule, TuiButton, RouterModule.forChild(ROUTES), i18nPipe],
  declarations: [HomePage],
})
export class HomePageModule {}
