import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { HomePage } from './home.page'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { WidgetListComponentModule } from 'src/app/components/widget-list/widget-list.component.module'

const routes: Routes = [
  {
    path: '',
    component: HomePage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
    WidgetListComponentModule,
  ],
  declarations: [HomePage],
})
export class HomePageModule {}
