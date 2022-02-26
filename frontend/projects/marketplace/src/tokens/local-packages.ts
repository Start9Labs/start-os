import { InjectionToken } from '@angular/core'
import { Observable, of } from 'rxjs'
import { LocalPackages } from '../types/local-packages'

export const LOCAL_PACKAGES = new InjectionToken<Observable<LocalPackages>>(
  '',
  {
    factory: () => of({}),
  },
)
