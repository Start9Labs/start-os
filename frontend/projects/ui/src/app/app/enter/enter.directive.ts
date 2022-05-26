import { Directive, HostListener, Inject } from '@angular/core'
import { DOCUMENT } from '@angular/common'
import { debounce } from '@start9labs/shared'

@Directive({
  selector: '[appEnter]',
})
export class EnterDirective {
  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  @HostListener('document:keydown.enter')
  @debounce()
  handleKeyboardEvent() {
    const elems = this.document.querySelectorAll('.enter-click')
    const elem = elems[elems.length - 1] as HTMLButtonElement

    if (elem && !elem.classList.contains('no-click') && !elem.disabled) {
      elem.click()
    }
  }
}
