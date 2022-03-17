import { AfterViewInit, Directive, ElementRef, Inject } from '@angular/core'
import { DOCUMENT } from '@angular/common'

// TODO: Refactor to use `MutationObserver` so it works with dynamic content
@Directive({
  selector: '[safeLinks]',
})
export class SafeLinksDirective implements AfterViewInit {
  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly elementRef: ElementRef<HTMLElement>,
  ) {}

  ngAfterViewInit() {
    Array.from(this.document.links)
      .filter(
        link =>
          link.hostname !== this.document.location.hostname &&
          this.elementRef.nativeElement.contains(link),
      )
      .forEach(link => {
        link.target = '_blank'
        link.setAttribute('rel', 'noreferrer')
        link.classList.add('externalLink')
      })
  }
}
