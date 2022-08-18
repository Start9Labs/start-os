import { Directive, ElementRef, Input } from '@angular/core'
import { ToastButton } from '@ionic/angular'

@Directive({
  selector: `button[toastButton], a[toastButton]`,
})
export class ToastButtonDirective implements ToastButton {
  @Input()
  icon?: string

  @Input()
  side?: 'start' | 'end'

  @Input()
  role?: 'cancel' | string

  handler = () => {
    this.elementRef.nativeElement.click()

    return false
  }

  constructor(private readonly elementRef: ElementRef<HTMLElement>) {}

  get text(): string | undefined {
    return this.elementRef.nativeElement.textContent?.trim() || undefined
  }

  get cssClass(): string[] {
    return Array.from(this.elementRef.nativeElement.classList)
  }
}
