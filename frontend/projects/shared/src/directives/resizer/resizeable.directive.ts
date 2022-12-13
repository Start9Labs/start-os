import { Directive, ElementRef } from '@angular/core'

@Directive({
  selector: '[resizeable]',
})
export class ResizeableDirective extends ElementRef<HTMLElement> {
  constructor({ nativeElement }: ElementRef<HTMLElement>) {
    super(nativeElement)
  }
}
