import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { TuiTilesModule } from '@taiga-ui/kit'
import { DesktopComponent } from './desktop.component'
import { CardComponent } from '../../components/card/card.component'
import { ToDesktopActionsPipe } from '../../pipes/to-desktop-actions'
import { ToDesktopItemPipe } from '../../pipes/to-desktop-item'

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
    TuiTilesModule,
    ToDesktopActionsPipe,
    ToDesktopItemPipe,
    RouterModule.forChild(ROUTES),
  ],
  declarations: [DesktopComponent],
  exports: [DesktopComponent],
})
export class DesktopModule {}
