import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  HostBinding,
  HostListener,
} from '@angular/core'

@Component({
  selector: '[ticker]',
  template: '<ng-content />',
  styles: `
    :host {
      max-width: 100%;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      transition: text-indent 1s;

      &:hover {
        text-indent: var(--indent, 0);
        text-overflow: clip;
      }
    }
  `,
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
