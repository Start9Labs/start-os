import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  AbstractControl,
  NonNullableFormBuilder,
  ReactiveFormsModule,
  ValidationErrors,
} from '@angular/forms'
import { TuiTime } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiCheckbox,
  TuiDialogContext,
  TuiError,
  TuiLabel,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiInputTime } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { BlackoutWindow } from './service'

@Component({
  template: `
    <form
      tuiForm="m"
      class="g-form"
      [formGroup]="form"
      (submit.prevent)="save()"
    >
      <fieldset>
        <legend>Time Window</legend>
        <tui-textfield>
          <label tuiLabel>Start Time</label>
          <input tuiInputTime formControlName="startTime" />
        </tui-textfield>
        <tui-textfield>
          <label tuiLabel>End Time</label>
          <input tuiInputTime formControlName="endTime" />
        </tui-textfield>
      </fieldset>
      <tui-error [formGroup]="form" />
      <fieldset formGroupName="days" [style.display]="'flex'">
        <legend>Days</legend>
        @for (day of displayDays; track day.key) {
          <label tuiLabel>
            <input type="checkbox" tuiCheckbox [formControlName]="day.key" />
            {{ day.label }}
          </label>
        }
      </fieldset>
      <footer>
        <button
          tuiButton
          type="button"
          appearance="flat"
          (click)="context.$implicit.complete()"
        >
          Cancel
        </button>
        <button tuiButton>Save</button>
      </footer>
    </form>
  `,
  providers: [
    tuiValidationErrorsProvider({
      endBeforeStart: 'End time must be later than start time',
    }),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiLabel,
    TuiCheckbox,
    TuiButton,
    TuiError,
    TuiInputTime,
  ],
})
class AddBlackoutWindow {
  protected readonly context = injectContext<TuiDialogContext<BlackoutWindow>>()

  // Display Mon–Sun; data keys map to Sun(0)–Sat(6)
  protected readonly displayDays = [
    { label: 'Mon', key: 1 },
    { label: 'Tue', key: 2 },
    { label: 'Wed', key: 3 },
    { label: 'Thu', key: 4 },
    { label: 'Fri', key: 5 },
    { label: 'Sat', key: 6 },
    { label: 'Sun', key: 0 },
  ]

  protected readonly form = inject(NonNullableFormBuilder).group(
    {
      startTime: [new TuiTime(22, 0)],
      endTime: [new TuiTime(23, 0)],
      days: inject(NonNullableFormBuilder).group({
        0: [false],
        1: [true],
        2: [true],
        3: [true],
        4: [true],
        5: [true],
        6: [false],
      }),
    },
    {
      validators: (control: AbstractControl): ValidationErrors | null => {
        const start = control.get('startTime')?.value
        const end = control.get('endTime')?.value

        return start && end && end <= start ? { endBeforeStart: true } : null
      },
    },
  )

  protected save(): void {
    if (this.form.invalid) return
    const val = this.form.getRawValue()
    const daysObj = val.days as Record<number, boolean>
    const days = Array.from({ length: 7 }, (_, i) => daysObj[i]) as [
      boolean,
      boolean,
      boolean,
      boolean,
      boolean,
      boolean,
      boolean,
    ]

    this.context.completeWith({
      startTime: val.startTime.toString('HH:MM'),
      endTime: val.endTime.toString('HH:MM'),
      days,
    })
  }
}

export const ADD_BLACKOUT_WINDOW = new PolymorpheusComponent(AddBlackoutWindow)
