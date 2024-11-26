import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  TemplateRef,
  ViewChild,
} from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { AbstractTuiNullableControl } from '@taiga-ui/legacy'
import { filter } from 'rxjs'
import { TuiAlertService, TuiDialogContext } from '@taiga-ui/core'
import { IST } from '@start9labs/start-sdk'
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
  T extends Exclude<IST.ValueSpec, IST.ValueSpecHidden>,
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
          status: 'warning',
          closeable: false,
          autoClose: 0,
        })
        .pipe(filter(Boolean), takeUntilDestroyed(this.destroyRef))
        .subscribe(() => {
          this.value = previous
        })
    }

    this.warned = true
    this.value = value === '' ? null : value
  }
}
