import { inject, StaticClassProvider } from '@angular/core'
import {
  catchError,
  EMPTY,
  exhaustMap,
  filter,
  from,
  interval,
  map,
  Observable,
  shareReplay,
  takeWhile,
} from 'rxjs'
import { SetupStatus } from '../types/api'
import { Constructor } from '../types/constructor'
import { ErrorService } from './error.service'

export function provideSetupService(
  api: Constructor<ConstructorParameters<typeof SetupService>[0]>,
): StaticClassProvider {
  return {
    provide: SetupService,
    deps: [api],
    useClass: SetupService,
  }
}

export class SetupService extends Observable<number> {
  private readonly errorService = inject(ErrorService)
  private readonly progress$ = interval(500).pipe(
    exhaustMap(() =>
      from(this.api.getSetupStatus()).pipe(
        catchError(e => {
          this.errorService.handleError(e)

          return EMPTY
        }),
      ),
    ),
    filter(Boolean),
    map(progress => {
      if (progress.complete) {
        return 1
      }

      return progress.totalBytes
        ? progress.bytesTransferred / progress.totalBytes
        : 0
    }),
    takeWhile(value => value !== 1, true),
    shareReplay(1),
  )

  constructor(
    private readonly api: { getSetupStatus: () => Promise<SetupStatus | null> },
  ) {
    super(subscriber => this.progress$.subscribe(subscriber))
  }
}
