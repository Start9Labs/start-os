import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { UntypedFormGroup } from '@angular/forms'
import { v4 } from 'uuid'
import { FormService } from 'src/app/services/form.service'
import { ValueSpecUnion, InputSpec } from 'start-sdk/types/config-types'

@Component({
  selector: 'form-union',
  templateUrl: './form-union.component.html',
  styleUrls: ['./form-union.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormUnionComponent {
  @Input() formGroup!: UntypedFormGroup
  @Input() spec!: ValueSpecUnion
  @Input() current?: Record<string, any>
  @Input() original?: Record<string, any>

  get selectedVariant(): string {
    return this.formGroup.get(this.spec.tag.id)?.value
  }

  get variantName(): string {
    return this.spec.tag['variant-names'][this.selectedVariant]
  }

  get variantSpec(): InputSpec {
    return this.spec.variants[this.selectedVariant]
  }

  get hasNewOptions(): boolean {
    // return Object.values(this.variantSpec).some(spec => spec['is-new'])
    return false
  }

  objectId = v4()

  constructor(private readonly formService: FormService) {}

  updateUnion(e: any): void {
    const tagId = this.spec.tag.id

    Object.keys(this.formGroup.controls).forEach(control => {
      if (control === tagId) return
      this.formGroup.removeControl(control)
    })

    const unionGroup = this.formService.getUnionObject(
      this.spec as ValueSpecUnion,
      e.detail.value,
    )

    Object.keys(unionGroup.controls).forEach(control => {
      if (control === tagId) return
      this.formGroup.addControl(control, unionGroup.controls[control])
    })
  }
}
