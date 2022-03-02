import { Directive, ElementRef, Inject } from '@angular/core'

@Directive({
  selector: '[elementRef]',
  exportAs: 'elementRef',
})
export class ElementDirective<T extends Element> extends ElementRef<T> {
  constructor(@Inject(ElementRef) { nativeElement }: ElementRef<T>) {
    super(nativeElement)
  }
}
