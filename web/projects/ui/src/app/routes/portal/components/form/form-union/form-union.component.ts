import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnChanges,
} from '@angular/core'
import { ControlContainer, FormGroupName } from '@angular/forms'
import { IST } from '@start9labs/start-sdk'
import { FormService } from 'src/app/services/form.service'
import { tuiPure } from '@taiga-ui/cdk'

@Component({
  selector: 'form-union',
  templateUrl: './form-union.component.html',
  styleUrls: ['./form-union.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  viewProviders: [
    {
      provide: ControlContainer,
      useExisting: FormGroupName,
    },
  ],
})
export class FormUnionComponent implements OnChanges {
  @Input({ required: true })
  spec!: IST.ValueSpecUnion

  selectSpec!: IST.ValueSpecSelect

  private readonly form = inject(FormGroupName)
  private readonly formService = inject(FormService)
  private readonly values: Record<string, any> = {}

  get union(): string {
    return this.form.value.selection
  }

  @tuiPure
  onUnion(union: string) {
    this.values[this.union] = this.form.control.controls['value']?.value
    this.form.control.setControl(
      'value',
      this.formService.getFormGroup(
        union ? this.spec.variants[union]?.spec || {} : {},
        [],
        this.values[union],
      ),
      {
        emitEvent: false,
      },
    )
  }

  ngOnChanges() {
    this.selectSpec = this.formService.getUnionSelectSpec(this.spec, this.union)
    if (this.union) this.onUnion(this.union)
  }
}
