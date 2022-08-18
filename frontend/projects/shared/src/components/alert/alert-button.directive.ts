import { Directive, ElementRef, Input } from '@angular/core'
import { AlertButton } from '@ionic/angular'

@Directive({
  selector: `button[alertButton], a[alertButton]`,
})
export class AlertButtonDirective implements AlertButton {
  @Input()
  icon?: string

  @Input()
  role?: 'cancel' | 'destructive' | string

  handler = () => {
    this.elementRef.nativeElement.click()

    return false
  }

  constructor(private readonly elementRef: ElementRef<HTMLElement>) {}

  get text(): string {
    return this.elementRef.nativeElement.textContent?.trim() || ''
  }

  get cssClass(): string[] {
    return Array.from(this.elementRef.nativeElement.classList)
  }
}
