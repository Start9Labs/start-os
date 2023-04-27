import { inject } from '@angular/core'
import { FormControlComponent } from './form-control/form-control.component'
import { ValueSpec } from 'start-sdk/lib/config/configTypes'

export abstract class Control<Spec extends ValueSpec, Value> {
  private readonly control: FormControlComponent<Spec, Value> =
    inject(FormControlComponent)

  get invalid(): boolean {
    return this.control.touched && this.control.invalid
  }

  get spec(): Spec {
    return this.control.spec
  }

  get value(): Value | null {
    return this.control.value
  }

  set value(value: Value | null) {
    this.control.onInput(value)
  }

  onFocus(focused: boolean) {
    this.control.onFocus(focused)
  }
}
