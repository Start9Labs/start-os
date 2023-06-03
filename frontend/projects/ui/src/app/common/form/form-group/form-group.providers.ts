import { Provider, SkipSelf } from '@angular/core'
import {
  TUI_ARROW_MODE,
  tuiInputDateOptionsProvider,
  tuiInputTimeOptionsProvider,
} from '@taiga-ui/kit'
import { TUI_DEFAULT_ERROR_MESSAGE } from '@taiga-ui/core'
import { ControlContainer } from '@angular/forms'
import { identity, of } from 'rxjs'

export const FORM_GROUP_PROVIDERS: Provider[] = [
  {
    provide: TUI_DEFAULT_ERROR_MESSAGE,
    useValue: of('Unknown error'),
  },
  {
    provide: ControlContainer,
    deps: [[new SkipSelf(), ControlContainer]],
    useFactory: identity,
  },
  {
    provide: TUI_ARROW_MODE,
    useValue: {
      interactive: null,
      disabled: null,
    },
  },
  tuiInputDateOptionsProvider({
    nativePicker: true,
  }),
  tuiInputTimeOptionsProvider({
    nativePicker: true,
  }),
]
