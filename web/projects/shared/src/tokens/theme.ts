import { InjectionToken } from '@angular/core'
import { EMPTY, Observable } from 'rxjs'

export const THEME = new InjectionToken<Observable<string>>('App theme', {
  factory: () => EMPTY,
})
