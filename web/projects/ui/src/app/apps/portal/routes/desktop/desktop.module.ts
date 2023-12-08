import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { DragScrollerDirective } from '@start9labs/shared'
import { TuiLoaderModule, TuiSvgModule } from '@taiga-ui/core'
import { TuiFadeModule } from '@taiga-ui/experimental'
import { TuiTilesModule } from '@taiga-ui/kit'
import { DesktopComponent } from './desktop.component'
import { CardComponent } from '../../components/card/card.component'
import { ToNavigationItemPipe } from '../../pipes/to-navigation-item'
import { ToBadgePipe } from '../../pipes/to-badge'
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
    ToNavigationItemPipe,
    RouterModule.forChild(ROUTES),
    TuiFadeModule,
    DragScrollerDirective,
    ToBadgePipe,
  ],
  declarations: [DesktopComponent],
  exports: [DesktopComponent],
})
export class DesktopModule {}
