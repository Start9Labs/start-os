import { inject, StaticClassProvider, Type } from '@angular/core'
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
import { SetupStatus } from '../types/setup-status'
import { ErrorToastService } from './error-toast.service'
import { Constructor } from '../types/constructor'

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
  private readonly errorToastService = inject(ErrorToastService)
  private readonly progress$ = interval(500).pipe(
    exhaustMap(() =>
      from(this.api.getSetupStatus()).pipe(
        catchError(e => {
          this.errorToastService.present(e)

          return EMPTY
        }),
      ),
    ),
    filter(Boolean),
    map(progress => {
      if (progress.complete) {
        return 1
      }

      return progress['total-bytes']
        ? progress['bytes-transferred'] / progress['total-bytes']
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
