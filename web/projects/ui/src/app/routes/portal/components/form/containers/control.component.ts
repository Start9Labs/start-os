import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { DialogService, i18nPipe } from '@start9labs/shared'
import { IST } from '@start9labs/start-sdk'
import { tuiAsControl, TuiControl } from '@taiga-ui/cdk'
import { TuiError } from '@taiga-ui/core'
import {
  TUI_FORMAT_ERROR,
  TUI_VALIDATION_ERRORS,
  TuiFieldErrorPipe,
} from '@taiga-ui/kit'
import { PolymorpheusOutlet } from '@taiga-ui/polymorpheus'

import { ControlSpec } from '../controls/control'
import { CONTROLS } from '../controls/controls'
import { ControlDirective } from './control.directive'

export const ERRORS = [
  'required',
  'pattern',
  'notNumber',
  'numberNotInteger',
  'numberNotInRange',
  'listNotUnique',
  'listNotInRange',
  'listItemIssue',
]

@Component({
  selector: 'form-control',
  template: `
    <ng-container *polymorpheusOutlet="controls[spec.type]" />
    <tui-error [error]="order | tuiFieldError | async" />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [
    tuiAsControl(FormControlComponent),
    {
      provide: TUI_VALIDATION_ERRORS,
      deps: [FormControlComponent],
      useFactory: (control: FormControlComponent<ControlSpec, string>) => ({
        [TUI_FORMAT_ERROR]: 'Invalid file format',
        required: 'Required',
        pattern: (context: any) =>
          'patterns' in control.spec &&
          getText(control.spec, String(context.requiredPattern)),
      }),
    },
  ],
  hostDirectives: [ControlDirective],
  imports: [AsyncPipe, PolymorpheusOutlet, TuiError, TuiFieldErrorPipe],
})
export class FormControlComponent<
  T extends ControlSpec,
  V,
> extends TuiControl<V | null> {
  private readonly dialogs = inject(DialogService)
  private readonly i18n = inject(i18nPipe)

  protected readonly controls = CONTROLS

  @Input({ required: true })
  spec!: T

  warned = false
  readonly order = ERRORS

  onInput(value: V | null) {
    const previous = this.value()
    const immutable =
      'immutable' in this.spec && this.spec.immutable
        ? `<p>${this.i18n.transform('This value cannot be changed once set')}</p>`
        : ''
    const warning = this.spec.warning + immutable

    if (!this.warned && warning) {
      this.dialogs
        .openConfirm({
          label: 'Warning',
          data: { content: warning as any, yes: 'Confirm', no: 'Cancel' },
          closeable: false,
          dismissible: false,
        })
        .subscribe(confirm => {
          if (!confirm) {
            this.onChange(previous)
          }
        })
    }

    this.warned = true
    this.onChange(value === '' ? null : value)
  }
}

function getText(
  { patterns }: IST.ValueSpecText | IST.ValueSpecTextarea,
  pattern: unknown,
): string {
  return (
    patterns?.find(({ regex }) => String(regex) === pattern)?.description ||
    'Invalid format'
  )
}
