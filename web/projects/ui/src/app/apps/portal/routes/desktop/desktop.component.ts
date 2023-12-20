import { CommonModule } from '@angular/common'
import {
  Component,
  ElementRef,
  inject,
  QueryList,
  ViewChild,
  ViewChildren,
} from '@angular/core'
import { RouterModule } from '@angular/router'
import { DragScrollerDirective } from '@start9labs/shared'
import { EMPTY_QUERY, TUI_PARENT_STOP } from '@taiga-ui/cdk'
import {
  tuiFadeIn,
  TuiLoaderModule,
  tuiScaleIn,
  TuiSvgModule,
} from '@taiga-ui/core'
import { TuiFadeModule } from '@taiga-ui/experimental'
import {
  TuiTileComponent,
  TuiTilesComponent,
  TuiTilesModule,
} from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { DesktopService } from '../../services/desktop.service'
import { DektopLoadingService } from './dektop-loading.service'
import { CardComponent } from '../../components/card.component'
import { DesktopItemDirective } from './desktop-item.directive'
import { ToNavigationItemPipe } from '../../pipes/to-navigation-item'
import { ToBadgePipe } from '../../pipes/to-badge'

@Component({
  standalone: true,
  templateUrl: 'desktop.component.html',
  styleUrls: ['desktop.component.scss'],
  animations: [TUI_PARENT_STOP, tuiScaleIn, tuiFadeIn],
  imports: [
    CommonModule,
    RouterModule,
    CardComponent,
    DesktopItemDirective,
    TuiSvgModule,
    TuiLoaderModule,
    TuiTilesModule,
    ToNavigationItemPipe,
    TuiFadeModule,
    DragScrollerDirective,
    ToBadgePipe,
  ],
})
export class DesktopComponent {
  @ViewChildren(TuiTileComponent, { read: ElementRef })
  private readonly tiles: QueryList<ElementRef> = EMPTY_QUERY

  readonly desktop = inject(DesktopService)
  readonly loading$ = inject(DektopLoadingService)
  readonly packages$ = inject(PatchDB<DataModel>).watch$('package-data')

  @ViewChild(TuiTilesComponent)
  readonly tile?: TuiTilesComponent

  onRemove() {
    const element = this.tile?.element
    const index = this.tiles
      .toArray()
      .map(({ nativeElement }) => nativeElement)
      .indexOf(element)

    this.desktop.remove(this.desktop.items[index])
  }

  onReorder(order: Map<number, number>) {
    this.desktop.reorder(order)
  }
}
