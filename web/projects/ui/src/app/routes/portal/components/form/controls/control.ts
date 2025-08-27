import { inject } from '@angular/core'
import { IST } from '@start9labs/start-sdk'
import { TuiControl } from '@taiga-ui/cdk'

export type ControlSpec = Exclude<
  IST.ValueSpec,
  | IST.ValueSpecHidden
  | IST.ValueSpecList
  | IST.ValueSpecUnion
  | IST.ValueSpecObject
>

export abstract class Control<Spec extends ControlSpec, Value> {
  public readonly control: any = inject(TuiControl)

  get spec(): Spec {
    return this.control.spec
  }

  get readOnly(): boolean {
    const def =
      'default' in this.spec &&
      this.spec.default != null &&
      this.spec.default !== this.value

    return (
      !!this.value &&
      !def &&
      !!this.control['control']?.pristine &&
      this.control.immutable
    )
  }

  get value(): Value | null {
    return this.control.value()
  }

  set value(value: Value | null) {
    this.control.onInput(value)
  }
}
