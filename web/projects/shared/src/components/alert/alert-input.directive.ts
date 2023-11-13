import { Directive, ElementRef, Input } from '@angular/core'
import { AlertInput } from '@ionic/angular'

@Directive({
  selector: `input[alertInput], textarea[alertInput]`,
})
export class AlertInputDirective<T> implements AlertInput {
  @Input()
  value?: T

  @Input()
  label?: string

  constructor(private readonly elementRef: ElementRef<HTMLInputElement>) {}

  get checked(): boolean {
    return this.elementRef.nativeElement.checked
  }

  get name(): string {
    return this.elementRef.nativeElement.name
  }

  get type(): AlertInput['type'] {
    return this.elementRef.nativeElement.type as AlertInput['type']
  }
}
