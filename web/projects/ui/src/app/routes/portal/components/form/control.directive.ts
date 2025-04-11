import { Directive, ElementRef, inject, OnDestroy, OnInit } from '@angular/core'
import { ControlContainer, NgControl } from '@angular/forms'
import { InvalidService } from './invalid.service'

@Directive({
  selector: 'form-control, form-array, form-object',
})
export class ControlDirective implements OnInit, OnDestroy {
  private readonly invalidService = inject(InvalidService, { optional: true })
  private readonly element: ElementRef<HTMLElement> = inject(ElementRef)
  private readonly control =
    inject(NgControl, { optional: true }) ||
    inject(ControlContainer, { optional: true })

  get invalid(): boolean {
    return !!this.control?.invalid
  }

  scrollIntoView() {
    this.element.nativeElement.scrollIntoView({ behavior: 'smooth' })
  }

  ngOnInit() {
    this.invalidService?.add(this)
  }

  ngOnDestroy() {
    this.invalidService?.remove(this)
  }
}
