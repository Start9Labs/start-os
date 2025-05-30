import { inject } from '@angular/core'
import { FormControlComponent } from './form-control/form-control.component'
import { IST } from '@start9labs/start-sdk'

export abstract class Control<
  Spec extends Exclude<IST.ValueSpec, IST.ValueSpecHidden>,
  Value,
> {
  private readonly control: FormControlComponent<Spec, Value> =
    inject(FormControlComponent)

  get invalid(): boolean {
    return this.control.touched && this.control.invalid
  }

  get spec(): Spec {
    return this.control.spec
  }

  get readOnly(): boolean {
    return (
      !!this.value && !!this.control.control?.pristine && this.control.immutable
    )
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
