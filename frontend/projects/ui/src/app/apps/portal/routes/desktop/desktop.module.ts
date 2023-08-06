import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { TuiLoaderModule, TuiSvgModule } from '@taiga-ui/core'
import { TuiTilesModule } from '@taiga-ui/kit'
import { DesktopComponent } from './desktop.component'
import { CardComponent } from '../../components/card/card.component'
import { ToDesktopItemPipe } from '../../pipes/to-desktop-item'
import { DesktopItemDirective } from './desktop-item.directive'

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
    DesktopItemDirective,
    TuiSvgModule,
    TuiLoaderModule,
    TuiTilesModule,
    ToDesktopItemPipe,
    RouterModule.forChild(ROUTES),
  ],
  declarations: [DesktopComponent],
  exports: [DesktopComponent],
})
export class DesktopModule {}
