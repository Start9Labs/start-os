import { Directive, inject } from '@angular/core'
import {
  ControlContainer,
  FormGroupDirective,
  FormGroupName,
} from '@angular/forms'
import { TuiAppearance } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm } from '@taiga-ui/layout'

@Directive({
  selector: '[formLoading]',
  host: { class: 'g-form', style: 'overflow: visible' },
  hostDirectives: [
    TuiForm,
    TuiCardLarge,
    TuiAppearance,
    {
      directive: TuiSkeleton,
      inputs: ['tuiSkeleton: formLoading'],
    },
  ],
})
export class Form {
  constructor() {
    inject(TuiCardLarge).space = 'compact'
  }
}

@Directive({
  hostDirectives: [TuiForm, TuiCardLarge],
})
export class FormSection {
  constructor() {
    inject(TuiCardLarge).space = 'compact'
  }
}

export const FORM = {
  provide: ControlContainer,
  useFactory: () =>
    inject(FormGroupDirective, { self: true, optional: true }) ||
    inject(FormGroupName, { self: true, optional: true }),
}
