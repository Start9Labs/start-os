import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { DesktopComponent } from './desktop.component'
import { CardComponent } from '../../components/card/card.component'
import { ToNavigationItemPipe } from '../../pipes/to-navigation-item'

const ROUTES: Routes = [
  {
    path: '',
    component: DesktopComponent,
  },
]

@NgModule({
  imports: [
    CommonModule,
    CardComponent,
    ToNavigationItemPipe,
    RouterModule.forChild(ROUTES),
  ],
  declarations: [DesktopComponent],
  exports: [DesktopComponent],
})
export class DesktopModule {}
