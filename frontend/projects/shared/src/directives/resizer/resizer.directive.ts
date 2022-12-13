import {
  Directive,
  ElementRef,
  HostBinding,
  HostListener,
  Inject,
  Input,
} from '@angular/core'

import { ResizeableDirective } from './resizeable.directive'

@Directive({
  selector: '[resizer]',
  host: {
    '[style.touchAction]': '"none"',
  },
})
export class ResizerDirective {
  @Input() resizer: [x: number, y: number] = [0, 0]

  x = NaN
  y = NaN
  width = 0
  height = 0

  constructor(
    @Inject(ResizeableDirective)
    private readonly resizeable: ElementRef<HTMLElement>,
  ) {}

  @HostBinding('style.cursor')
  get cursor(): string {
    if (!this.resizer[0]) {
      return 'ns-resize'
    }

    if (!this.resizer[1]) {
      return 'ew-resize'
    }

    if (this.resizer[0] * this.resizer[1] > 0) {
      return 'nwse-resize'
    }

    return 'nesw-resize'
  }

  @HostListener('pointerdown.silent.prevent', ['$event'])
  onMouseDown({ x, y }: MouseEvent): void {
    this.x = x
    this.y = y
    this.width = this.resizeable.nativeElement.clientWidth
    this.height = this.resizeable.nativeElement.clientHeight
  }

  @HostListener('document:pointermove.silent', ['$event'])
  onMouseMove({ x, y, buttons }: MouseEvent): void {
    if (isNaN(this.x) || isNaN(this.y) || !buttons) {
      return this.onMouseUp()
    }

    const { style } = this.resizeable.nativeElement

    style.width = `${this.width + this.resizer[0] * (x - this.x)}px`
    style.height = `${this.height + this.resizer[1] * (y - this.y)}px`
  }

  @HostListener('document:pointerup.silent')
  onMouseUp(): void {
    this.x = NaN
    this.y = NaN
  }
}
