import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  DestroyRef,
  inject,
  Input,
  TemplateRef,
  ViewChild,
} from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { i18nPipe } from '@start9labs/shared'
import { IST } from '@start9labs/start-sdk'
import { tuiAsControl, TuiControl } from '@taiga-ui/cdk'
import {
  TuiAlertService,
  TuiButton,
  TuiDialogContext,
  TuiError,
} from '@taiga-ui/core'
import {
  TUI_FORMAT_ERROR,
  TUI_VALIDATION_ERRORS,
  TuiFieldErrorPipe,
} from '@taiga-ui/kit'
import { PolymorpheusOutlet } from '@taiga-ui/polymorpheus'
import { filter } from 'rxjs'

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
    @if (spec.warning || immutable) {
      <ng-template #warning let-completeWith="completeWith">
        {{ spec.warning }}
        @if (immutable) {
          <p>{{ 'This value cannot be changed once set' | i18n }}!</p>
        }
        <div [style.margin-top.rem]="0.5">
          <button
            tuiButton
            type="button"
            appearance="secondary-grayscale"
            size="s"
            [style.margin-inline-end.rem]="0.5"
            (click)="completeWith(false)"
          >
            {{ 'Continue' | i18n }}
          </button>
          <button
            tuiButton
            type="button"
            appearance="flat-grayscale"
            size="s"
            (click)="completeWith(true)"
          >
            {{ 'Cancel' | i18n }}
          </button>
        </div>
      </ng-template>
    }
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
  imports: [
    AsyncPipe,
    i18nPipe,
    PolymorpheusOutlet,
    TuiError,
    TuiFieldErrorPipe,
    TuiButton,
  ],
})
export class FormControlComponent<
  T extends ControlSpec,
  V,
> extends TuiControl<V | null> {
  private readonly destroyRef = inject(DestroyRef)
  private readonly alerts = inject(TuiAlertService)
  private readonly i18n = inject(i18nPipe)

  protected readonly controls = CONTROLS

  @Input({ required: true })
  spec!: T

  @ViewChild('warning')
  warning?: TemplateRef<TuiDialogContext<boolean>>

  warned = false
  readonly order = ERRORS

  get immutable(): boolean {
    return 'immutable' in this.spec && this.spec.immutable
  }

  onInput(value: V | null) {
    const previous = this.value()

    if (!this.warned && this.warning) {
      this.alerts
        .open<boolean>(this.warning, {
          label: this.i18n.transform('Warning'),
          appearance: 'warning',
          closeable: false,
          autoClose: 0,
        })
        .pipe(filter(Boolean), takeUntilDestroyed(this.destroyRef))
        .subscribe(() => {
          this.onChange(previous)
        })
    }

    this.warned = true
    this.onChange(value === '' ? null : value)
  }
}

function getText({ patterns }: IST.ValueSpecText, pattern: unknown): string {
  return (
    patterns?.find(({ regex }) => String(regex) === pattern)?.description ||
    'Invalid format'
  )
}
