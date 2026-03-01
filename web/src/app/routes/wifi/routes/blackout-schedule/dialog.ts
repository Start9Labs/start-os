import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  AbstractControl,
  NonNullableFormBuilder,
  ReactiveFormsModule,
  ValidationErrors,
} from '@angular/forms'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTime } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiGroup,
  TuiLabel,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiBlock, TuiInputTime } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { filter } from 'rxjs'
import { provideHelp } from 'src/app/help/help'
import { ModalHelp } from 'src/app/help/modal-help'
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
        <div tuiGroup [collapsed]="true">
          @for (day of displayDays; track day.key) {
            <label tuiBlock="s" appearance="">
              <input type="checkbox" tuiBlock="s" [formControlName]="day.key" />
              {{ day.label }}
            </label>
          }
        </div>
      </fieldset>
      <footer>
        @if (context.label === 'Edit Blackout Window') {
          <button
            tuiButton
            type="button"
            appearance="secondary-destructive"
            [style.margin-inline-end]="'auto'"
            (click)="remove()"
          >
            Remove
          </button>
        }
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
    [tuiGroup] {
      width: 100%;
      height: var(--tui-height-m);
    }
  `,
  hostDirectives: [ModalHelp],
  providers: [
    provideHelp('/wifi/blackout-schedule/dialog'),
    tuiValidationErrorsProvider({
      endBeforeStart: 'End time must be later than start time',
    }),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiLabel,
    TuiButton,
    TuiError,
    TuiInputTime,
    TuiGroup,
    TuiBlock,
  ],
})
class AddBlackoutWindow {
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly builder = inject(NonNullableFormBuilder)
  protected readonly context =
    injectContext<TuiDialogContext<BlackoutWindow | null, BlackoutWindow>>()

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

  protected readonly form = this.builder.group(
    {
      startTime: [TuiTime.fromString(this.context.data.startTime)],
      endTime: [TuiTime.fromString(this.context.data.endTime)],
      days: this.builder.group({
        0: [this.context.data.days[0]],
        1: [this.context.data.days[1]],
        2: [this.context.data.days[2]],
        3: [this.context.data.days[3]],
        4: [this.context.data.days[4]],
        5: [this.context.data.days[5]],
        6: [this.context.data.days[6]],
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

  protected remove(): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.context.completeWith(null)
      })
  }
}

export const ADD_BLACKOUT_WINDOW = new PolymorpheusComponent(AddBlackoutWindow)
