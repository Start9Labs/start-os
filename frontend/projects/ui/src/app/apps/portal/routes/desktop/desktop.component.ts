import {
  Component,
  ElementRef,
  inject,
  QueryList,
  ViewChild,
  ViewChildren,
} from '@angular/core'
import { EMPTY_QUERY, TUI_PARENT_STOP } from '@taiga-ui/cdk'
import { tuiFadeIn, tuiScaleIn } from '@taiga-ui/core'
import { TuiTileComponent, TuiTilesComponent } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { DesktopService } from '../../services/desktop.service'
import { Observable } from 'rxjs'
import { DektopLoadingService } from './dektop-loading.service'

@Component({
  templateUrl: 'desktop.component.html',
  styleUrls: ['desktop.component.scss'],
  animations: [TUI_PARENT_STOP, tuiScaleIn, tuiFadeIn],
})
export class DesktopComponent {
  @ViewChildren(TuiTileComponent, { read: ElementRef })
  private readonly tiles: QueryList<ElementRef> = EMPTY_QUERY

  readonly desktop = inject(DesktopService)
  readonly loading$ = inject(DektopLoadingService)
  readonly packages$: Observable<Record<string, PackageDataEntry>> =
    inject<PatchDB<DataModel>>(PatchDB).watch$('package-data')

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
