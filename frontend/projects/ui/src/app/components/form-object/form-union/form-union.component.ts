import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { AbstractControl, UntypedFormGroup } from '@angular/forms'
import { v4 } from 'uuid'
import { FormService } from 'src/app/services/form.service'
import {
  ValueSpecUnion,
  InputSpec,
  unionSelectKey,
} from 'start-sdk/lib/config/configTypes'

@Component({
  selector: 'form-union',
  templateUrl: './form-union.component.html',
  styleUrls: ['./form-union.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormUnionComponent {
  readonly unionSelectKey = unionSelectKey

  @Input() formGroup!: UntypedFormGroup
  @Input() spec!: ValueSpecUnion
  @Input() current?: Record<string, any>
  @Input() original?: Record<string, any>

  get unionControl(): AbstractControl | null {
    return this.formGroup.get(unionSelectKey)
  }

  get selectedVariant(): string {
    return this.unionControl?.value || ''
  }

  get variantName(): string {
    return this.spec.variants[this.selectedVariant]?.name || ''
  }

  get variantSpec(): InputSpec {
    return this.spec.variants[this.selectedVariant]?.spec || {}
  }

  get hasNewOptions(): boolean {
    // return Object.values(this.variantSpec).some(spec => spec['is-new'])
    return false
  }

  objectId = v4()

  constructor(private readonly formService: FormService) {}

  updateUnion(e: any): void {
    Object.keys(this.formGroup.controls).forEach(control => {
      if (control === unionSelectKey) return
      this.formGroup.removeControl(control)
    })

    const unionGroup = this.formService.getUnionObject(
      this.spec as ValueSpecUnion,
      e.detail.value,
    )

    Object.keys(unionGroup.controls).forEach(control => {
      if (control === unionSelectKey) return
      this.formGroup.addControl(control, unionGroup.controls[control])
    })
  }
}
