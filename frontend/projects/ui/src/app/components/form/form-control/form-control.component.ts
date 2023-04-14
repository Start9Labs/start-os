import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  TemplateRef,
  ViewChild,
} from '@angular/core'
import { AbstractTuiNullableControl } from '@taiga-ui/cdk'
import {
  TuiAlertService,
  TuiDialogContext,
  TuiNotification,
} from '@taiga-ui/core'
import { TUI_VALIDATION_ERRORS } from '@taiga-ui/kit'
import { filter, takeUntil } from 'rxjs'
import { ValueSpec, ValueSpecString } from 'start-sdk/lib/config/configTypes'
import { ERRORS } from '../form-group/form-group.component'

@Component({
  selector: 'form-control',
  templateUrl: './form-control.component.html',
  styleUrls: ['./form-control.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      deps: [FormControlComponent],
      useFactory: (control: FormControlComponent<ValueSpecString, string>) => ({
        required: 'Required',
        pattern: () => control.spec.patternDescription,
      }),
    },
  ],
})
export class FormControlComponent<
  T extends ValueSpec,
  V,
> extends AbstractTuiNullableControl<V> {
  @Input()
  spec!: T

  @ViewChild('warning')
  warning?: TemplateRef<TuiDialogContext<boolean>>

  warned = false
  focused = false
  readonly order = ERRORS
  private readonly alerts = inject(TuiAlertService)

  onFocus(focused: boolean) {
    this.focused = focused
    this.updateFocused(focused)
  }

  onInput(value: V | null) {
    const previous = this.value

    if (!this.warned && this.warning) {
      this.alerts
        .open<boolean>(this.warning, {
          label: 'Warning',
          status: TuiNotification.Warning,
          hasCloseButton: false,
          autoClose: false,
        })
        .pipe(filter(Boolean), takeUntil(this.destroy$))
        .subscribe(() => {
          this.value = previous
        })
    }

    this.warned = true
    this.value = value === '' ? null : value
  }
}
