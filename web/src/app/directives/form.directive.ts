import { Directive, inject } from '@angular/core'
import { TuiAppearance } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm } from '@taiga-ui/layout'

@Directive({
  host: { class: 'g-form' },
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
