import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  AbstractControl,
  NonNullableFormBuilder,
  ReactiveFormsModule,
  ValidationErrors,
} from '@angular/forms'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiLabel,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiCheckbox } from '@taiga-ui/kit'
import { TuiForm, TuiHeader } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { BlackoutWindow } from './service'

@Component({
  template: `
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <header tuiHeader="h6"><h2 tuiTitle>Time Window</h2></header>
      <div class="time-row">
        <div class="time-field">
          <label tuiLabel>Start Time</label>
          <input type="time" formControlName="startTime" />
        </div>
        <div class="time-field">
          <label tuiLabel>End Time</label>
          <input type="time" formControlName="endTime" />
        </div>
      </div>
      @if (form.errors?.['endBeforeStart']) {
        <tui-error>
          <span class="t-message">End time must be later than start time</span>
        </tui-error>
      }
      <header tuiHeader="h6"><h2 tuiTitle>Days</h2></header>
      <div class="days" formGroupName="days">
        @for (day of displayDays; track day.key) {
          <label tuiLabel>
            <input type="checkbox" tuiCheckbox [formControlName]="day.key" />
            {{ day.label }}
          </label>
        }
      </div>
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
  styles: `
    .time-row {
      display: flex;
      flex-wrap: wrap;
      gap: 1rem;
    }

    .time-field {
      flex: 1;
      min-width: 10rem;
    }

    .time-field input {
      width: 100%;
    }

    .days {
      display: flex;
      flex-wrap: wrap;
      gap: 1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiHeader,
    TuiTitle,
    TuiLabel,
    TuiCheckbox,
    TuiButton,
    TuiError,
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
      startTime: ['22:00'],
      endTime: ['23:00'],
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
      startTime: val.startTime,
      endTime: val.endTime,
      days,
    })
  }
}

export const ADD_BLACKOUT_WINDOW = new PolymorpheusComponent(AddBlackoutWindow)
