import { inject, Type } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TuiNotificationService } from '@taiga-ui/core'
import {
  catchError,
  EMPTY,
  firstValueFrom,
  from,
  merge,
  share,
  Subject,
  switchMap,
  timer,
} from 'rxjs'
import { ActionService } from 'src/app/services/action.service'
import {
  isNetworkError,
  NetworkRestartService,
} from 'src/app/services/network-restart.service'

export type FormRawValue<T> = T extends { getRawValue(): infer R } ? R : never

export abstract class FormService<T> {
  private readonly load$ = new Subject<void>()
  private readonly alerts = inject(TuiNotificationService)
  private readonly networkRestart = inject(NetworkRestartService)
  private readonly value$ = merge(this.load$, timer(0, 5000)).pipe(
    switchMap(() =>
      from(this.load()).pipe(
        catchError(e => {
          if (isNetworkError(e) && this.networkRestart.isSuppressed) {
            return EMPTY
          }
          return this.alerts.open<never>(e, { appearance: 'negative' })
        }),
      ),
    ),
    share(),
  )

  protected readonly actions = inject(ActionService)
  readonly data = toSignal(this.value$)

  abstract load(): Promise<T>
  abstract store(data: T): Promise<void>

  refresh(): void {
    this.load$.next()
  }

  protected async refreshAndWait(): Promise<void> {
    this.refresh()
    await firstValueFrom(this.value$)
  }

  async save(data: T): Promise<boolean> {
    return this.actions.run(async () => {
      await this.store(data)
      await this.refreshAndWait()
    })
  }
}

export function provideFormService<T = any>(useExisting: Type<FormService<T>>) {
  return [useExisting, { provide: FormService, useExisting }]
}

export function injectFormService<T>(): FormService<T> {
  return inject(FormService)
}
