import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { UntypedFormGroup } from '@angular/forms'
import { v4 } from 'uuid'
import { FormService } from 'src/app/services/form.service'
import { ValueSpecUnion } from 'start-sdk/types/config-types'

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

  get unionValue() {
    return this.formGroup.get(this.spec.tag.id)?.value
  }

  get isNew() {
    return !this.original
  }

  get hasNewOptions() {
    const tagId = this.spec.tag.id
    return (
      this.original?.[tagId] === this.current?.[tagId] &&
      !!Object.keys(this.current || {}).find(
        key => this.original![key] === undefined,
      )
    )
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
