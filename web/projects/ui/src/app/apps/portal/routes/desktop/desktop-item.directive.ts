import {
  Directive,
  ElementRef,
  HostBinding,
  inject,
  Input,
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
export class DesktopItemDirective implements OnInit {
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
}
