import { Directive, ElementRef, inject, OnDestroy, OnInit } from '@angular/core'
import { ControlContainer, NgControl } from '@angular/forms'
import { InvalidService } from './invalid.service'
import { FormObjectComponent } from './form-object/form-object.component'

@Directive({
  selector: 'form-control, form-array',
})
export class ControlDirective implements OnInit, OnDestroy {
  private readonly object = inject(FormObjectComponent, { optional: true })
  private readonly invalidService = inject(InvalidService, { optional: true })
  private readonly element: ElementRef<HTMLElement> = inject(ElementRef)
  private readonly control =
    inject(NgControl, { optional: true }) ||
    inject(ControlContainer, { optional: true })

  get invalid(): boolean {
    return !!this.control?.invalid
  }

  scrollIntoView() {
    this.object?.expandAll()
    setTimeout(
      () => this.element.nativeElement.scrollIntoView({ behavior: 'smooth' }),
      this.object ? 500 : 0,
    )
  }

  ngOnInit() {
    this.invalidService?.add(this)
  }

  ngOnDestroy() {
    this.invalidService?.remove(this)
  }
}
