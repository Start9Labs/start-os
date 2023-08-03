import {
  Directive,
  ElementRef,
  HostListener,
  inject,
  Input,
} from '@angular/core'
import { tuiGetActualTarget, tuiIsElement, tuiPx } from '@taiga-ui/cdk'
import { DrawerComponent } from './drawer.component'
import { DesktopService } from '../../services/desktop.service'

/**
 * This directive is responsible for drag and drop of the drawer item.
 * It saves item to desktop when dropped.
 */
@Directive({
  selector: '[drawerItem]',
  standalone: true,
  host: {
    '[style.touchAction]': '"none"',
  },
})
export class DrawerItemDirective {
  private readonly desktop = inject(DesktopService)
  private readonly drawer = inject(DrawerComponent)
  private readonly element: HTMLElement = inject(ElementRef).nativeElement

  private x = NaN
  private y = NaN

  @Input()
  drawerItem = ''

  @HostListener('pointerdown.prevent.silent', ['$event'])
  onStart(event: PointerEvent): void {
    // This element is already on the desktop
    if (this.desktop.items.includes(this.drawerItem)) return

    const target = tuiGetActualTarget(event)
    const { x, y, pointerId } = event
    const { left, top } = this.element.getBoundingClientRect()

    if (tuiIsElement(target)) {
      target.releasePointerCapture(pointerId)
    }

    this.drawer.open = false
    this.onPointer(x - left, y - top)
  }

  @HostListener('document:pointerup.silent')
  onPointer(x = NaN, y = NaN): void {
    // Some other element is dragged
    if (Number.isNaN(this.x) && Number.isNaN(x)) return

    this.x = x
    this.y = y
    this.process(NaN, NaN)
  }

  @HostListener('document:pointermove.silent', ['$event.x', '$event.y'])
  onMove(x: number, y: number): void {
    // This element is not dragged
    if (Number.isNaN(this.x)) return

    this.process(x, y)
    this.desktop.add('')
  }

  private process(x: number, y: number) {
    const { style } = this.element
    const { items } = this.desktop
    const dragged = !Number.isNaN(this.x + x)

    style.pointerEvents = dragged ? 'none' : ''
    style.position = dragged ? 'fixed' : ''
    style.top = dragged ? tuiPx(y - this.y) : ''
    style.left = dragged ? tuiPx(x - this.x) : ''

    if (dragged || !items.includes('')) {
      return
    }

    this.desktop.items = items.map(item => item || this.drawerItem)
    this.desktop.reorder(this.desktop.order)
  }
}
