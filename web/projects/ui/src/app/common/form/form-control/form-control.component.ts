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
import { filter, takeUntil } from 'rxjs'
import { ValueSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { ERRORS } from '../form-group/form-group.component'
import { FORM_CONTROL_PROVIDERS } from './form-control.providers'

@Component({
  selector: 'form-control',
  templateUrl: './form-control.component.html',
  styleUrls: ['./form-control.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: FORM_CONTROL_PROVIDERS,
})
export class FormControlComponent<
  T extends ValueSpec,
  V,
> extends AbstractTuiNullableControl<V> {
  @Input({ required: true })
  spec!: T

  @ViewChild('warning')
  warning?: TemplateRef<TuiDialogContext<boolean>>

  warned = false
  focused = false
  readonly order = ERRORS
  private readonly alerts = inject(TuiAlertService)

  get immutable(): boolean {
    return 'immutable' in this.spec && this.spec.immutable
  }

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
