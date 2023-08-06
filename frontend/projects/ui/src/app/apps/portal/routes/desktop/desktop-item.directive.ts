import {
  Directive,
  ElementRef,
  HostBinding,
  inject,
  Input,
  OnDestroy,
  OnInit,
} from '@angular/core'
import { TuiTilesComponent } from '@taiga-ui/kit'

/**
 * This directive is responsible for creating empty placeholder
 * on the desktop when item is dragged from the drawer
 */
@Directive({
  selector: '[desktopItem]',
  standalone: true,
})
export class DesktopItemDirective implements OnInit, OnDestroy {
  private readonly element: Element = inject(ElementRef).nativeElement
  private readonly tiles = inject(TuiTilesComponent)

  @Input()
  desktopItem = ''

  @HostBinding('class._empty')
  get empty(): boolean {
    return !this.desktopItem
  }

  ngOnInit() {
    if (this.empty) this.tiles.element = this.element
  }

  // TODO: Remove after Taiga UI updated to 3.40.0
  ngOnDestroy() {
    if (this.tiles.element === this.element) this.tiles.element = null
  }
}
