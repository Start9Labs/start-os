import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  inject,
  Input,
  Output,
} from '@angular/core'
import { ControlContainer } from '@angular/forms'
import { ValueSpecObject } from '@start9labs/start-sdk/lib/config/configTypes'

@Component({
  selector: 'form-object',
  templateUrl: './form-object.component.html',
  styleUrls: ['./form-object.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormObjectComponent {
  @Input({ required: true })
  spec!: ValueSpecObject

  @Input()
  open = false

  @Output()
  readonly openChange = new EventEmitter<boolean>()

  private readonly container = inject(ControlContainer)

  get invalid() {
    return !this.container.valid && this.container.touched
  }

  toggle() {
    this.open = !this.open
    this.openChange.emit(this.open)
  }
}
