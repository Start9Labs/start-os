import { InjectionToken } from '@angular/core'
import { Observable, of } from 'rxjs'

export const LOAD_TRIGGER = new InjectionToken<Observable<unknown>>(
  'Signal to start loading marketplace data',
  {
    factory: () => of(null),
  },
)
