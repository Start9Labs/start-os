import { inject, Type } from '@angular/core'
import {
  takeUntilDestroyed,
  toObservable,
  toSignal,
} from '@angular/core/rxjs-interop'
import { TuiAlertService } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import {
  catchError,
  merge,
  skip,
  Subject,
  switchMap,
  switchMapTo,
  takeUntil,
  timer,
} from 'rxjs'

export type FormRawValue<T> = T extends { getRawValue(): infer R } ? R : never

export abstract class FormService<T> {
  private readonly load$ = new Subject<void>()
  private readonly alerts = inject(TuiAlertService)

  readonly data = toSignal(
    merge(this.load$, timer(0, 5000)).pipe(
      switchMap(() => this.load()),
      catchError(e => this.alerts.open<never>(e, { appearance: 'negative' })),
    ),
  )

  private readonly saving$ = inject(TuiNotificationMiddleService)
    .open('Saving')
    .pipe(
      takeUntil(this.load$.pipe(switchMapTo(toObservable(this.data)), skip(1))),
      takeUntilDestroyed(),
    )

  abstract load(): Promise<T>
  abstract store(data: T): Promise<void>

  async save(data: T): Promise<boolean> {
    const saving = this.saving$.subscribe()

    try {
      await this.store(data)
      this.load$.next()

      return true
    } catch (e: any) {
      console.error(e)
      this.alerts.open(e, { appearance: 'negative' }).subscribe()
      saving.unsubscribe()

      return false
    }
  }
}

export function provideFormService<T = any>(useClass: Type<FormService<T>>) {
  return { provide: FormService, useClass }
}

export function injectFormService<T>(): FormService<T> {
  return inject(FormService)
}
