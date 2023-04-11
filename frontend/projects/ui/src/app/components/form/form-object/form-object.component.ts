import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  inject,
  Input,
} from '@angular/core'
import { ControlContainer } from '@angular/forms'
import { ValueSpecObject } from 'start-sdk/lib/config/configTypes'

@Component({
  selector: 'form-object',
  templateUrl: './form-object.component.html',
  styleUrls: ['./form-object.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormObjectComponent {
  @Input()
  spec!: ValueSpecObject

  open = false

  private readonly element: ElementRef<HTMLElement> = inject(ElementRef)
  private readonly container = inject(ControlContainer)
  private readonly parent = inject(FormObjectComponent, {
    optional: true,
    skipSelf: true,
  })

  get invalid() {
    return !this.container.valid && this.container.touched
  }

  expandAll() {
    this.open = true
    this.parent?.expandAll()

    if (!this.parent)
      this.element.nativeElement.scrollIntoView({ behavior: 'smooth' })
  }
}
