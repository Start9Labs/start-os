import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { ServerShowPage } from './server-show.page'
import { FormsModule } from '@angular/forms'
import { TextSpinnerComponentModule } from '@start9labs/shared'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { SnakePageModule } from 'src/app/modals/snake/snake.module'

const routes: Routes = [
  {
    path: '',
    component: ServerShowPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    RouterModule.forChild(routes),
    TextSpinnerComponentModule,
    BadgeMenuComponentModule,
    SnakePageModule,
  ],
  declarations: [ServerShowPage],
})
export class ServerShowPageModule {}
