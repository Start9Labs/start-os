import { inject } from '@angular/core'
import { MonoTypeOperatorFunction } from 'rxjs'
import { catchError, filter, skip, switchMap, take } from 'rxjs/operators'
import { StateService } from 'src/app/services/state.service'

export function retryWithState<T>(
  state = inject(StateService),
): MonoTypeOperatorFunction<T> {
  // @TODO Matt should we inspect an error somehow to make sure this is what we want?
  return catchError((_, original$) => {
    state.retrigger()

    return state.pipe(
      skip(1), // skipping previous value stored due to shareReplay
      filter(current => current === 'running'),
      take(1),
      switchMap(() => original$),
    )
  })
}
