import {
  Directive,
  ElementRef,
  HostListener,
  inject,
  NgZone,
} from '@angular/core'
import { ANIMATION_FRAME } from '@ng-web-apis/common'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { tuiZonefree } from '@taiga-ui/cdk'
import { filter } from 'rxjs'

const SIZE = 100
const SPEED = 15

@Directive({
  selector: '[dragScroller]',
  standalone: true,
})
export class DragScrollerDirective {
  private readonly element: HTMLElement = inject(ElementRef).nativeElement
  private dragging = false
  private x = 0
  private y = 0

  private readonly sub = inject(ANIMATION_FRAME)
    .pipe(
      filter(() => this.dragging),
      tuiZonefree(inject(NgZone)),
      takeUntilDestroyed(),
    )
    .subscribe(() => {
      this.element.scrollTop += this.y * SPEED
      this.element.scrollLeft += this.x * SPEED
    })

  @HostListener('document:pointerdown.silent', ['true'])
  @HostListener('document:pointerup.silent', ['false'])
  onPointer(dragging: boolean) {
    this.dragging = dragging
    this.x = 0
    this.y = 0
  }

  @HostListener('pointermove.silent', ['$event'])
  onPointerMove(event: PointerEvent) {
    if (!this.dragging) {
      return
    }

    const { clientX, clientY } = event
    const { top, left, right, bottom } = this.element.getBoundingClientRect()
    const x = Math.min(clientX - left, SIZE) - Math.min(right - clientX, SIZE)
    const y = Math.min(clientY - top, SIZE) - Math.min(bottom - clientY, SIZE)

    this.x = x / SIZE
    this.y = y / SIZE
  }
}
