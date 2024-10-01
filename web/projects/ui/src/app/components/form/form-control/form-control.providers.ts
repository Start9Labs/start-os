import { forwardRef, Provider } from '@angular/core'
import { TUI_VALIDATION_ERRORS } from '@taiga-ui/kit'
import { IST } from '@start9labs/start-sdk'
import { FormControlComponent } from './form-control.component'

interface ValidatorsPatternError {
  actualValue: string
  requiredPattern: string | RegExp
}

export const FORM_CONTROL_PROVIDERS: Provider[] = [
  {
    provide: TUI_VALIDATION_ERRORS,
    deps: [forwardRef(() => FormControlComponent)],
    useFactory: (control: FormControlComponent<Exclude<IST.ValueSpec, IST.ValueSpecHidden>, string>) => ({
      required: 'Required',
      pattern: ({ requiredPattern }: ValidatorsPatternError) =>
        ('patterns' in control.spec &&
          control.spec.patterns.find(
            ({ regex }) => String(regex) === String(requiredPattern),
          )?.description) ||
        'Invalid format',
    }),
  },
]
