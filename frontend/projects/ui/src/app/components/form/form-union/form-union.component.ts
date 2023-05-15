import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnChanges,
} from '@angular/core'
import { ControlContainer, FormGroupName } from '@angular/forms'
import {
  unionSelectKey,
  ValueSpecSelect,
  ValueSpecUnion,
  unionValueKey,
} from '@start9labs/start-sdk/lib/config/configTypes'
import { FormService } from '../../../services/form.service'
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
  @Input()
  spec!: ValueSpecUnion

  selectSpec!: ValueSpecSelect

  readonly select = unionSelectKey
  readonly value = unionValueKey

  private readonly form = inject(FormGroupName)
  private readonly formService = inject(FormService)

  get union(): string {
    return this.form.value['unionSelectKey']
  }

  @tuiPure
  onUnion(union: string) {
    this.form.control.setControl(
      'unionValueKey',
      this.formService.getFormGroup(
        union ? this.spec.variants[union].spec : {},
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
