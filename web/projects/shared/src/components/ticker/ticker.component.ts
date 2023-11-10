import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  HostBinding,
  HostListener,
} from '@angular/core'

@Component({
  selector: '[ticker]',
  template: '<ng-content></ng-content>',
  styleUrls: ['./ticker.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TickerComponent {
  constructor(private readonly elementRef: ElementRef<HTMLElement>) {}

  @HostBinding('style.--indent.px')
  indent = 0

  @HostListener('mouseenter')
  onMouseEnter() {
    const { scrollWidth, clientWidth } = this.elementRef.nativeElement

    this.indent = Math.ceil(clientWidth - scrollWidth)
  }
}
