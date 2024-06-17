import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { Pipe, PipeTransform } from '@angular/core'
import {
  EMPTY,
  Observable,
  catchError,
  filter,
  from,
  interval,
  map,
  of,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'
import { ErrorToastService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'

@Component({
  selector: 'app-loading',
  templateUrl: 'loading.page.html',
  styleUrls: ['loading.page.scss'],
})
export class LoadingPage {
  readonly progress$ = this.getRunningStatus$().pipe(
    switchMap(res =>
      this.api.openProgressWebsocket$(res.guid).pipe(
        startWith(res.progress),
        catchError((_, watch$) => {
          return interval(2000).pipe(
            switchMap(() =>
              from(this.api.getStatus()).pipe(catchError(() => EMPTY)),
            ),
            take(1),
            switchMap(() => watch$),
          )
        }),
        tap(progress => {
          if (progress.overall === true) {
            this.getStatus()
          }
        }),
      ),
    ),
    map(({ phases, overall }) => {
      return {
        total: getDecimal(overall),
        message: phases
          .filter(
            (
              p,
            ): p is {
              name: string
              progress: {
                done: number
                total: number | null
              }
            } => p.progress !== true && p.progress !== null,
          )
          .map(p => `${p.name}: (${getPhaseBytes(p.progress)})`)
          .join(','),
      }
    }),
  )

  constructor(
    private readonly navCtrl: NavController,
    private readonly api: ApiService,
    private readonly errorToastService: ErrorToastService,
  ) {}

  private async getStatus(): Promise<{
    status: 'running'
    guid: string
    progress: T.FullProgress
  } | void> {
    const res = await this.api.getStatus()

    if (!res) {
      this.navCtrl.navigateRoot('/home')
    } else if (res.status === 'complete') {
      this.navCtrl.navigateForward(`/success`)
    } else {
      return res
    }
  }

  private getRunningStatus$(): Observable<{
    status: 'running'
    guid: string
    progress: T.FullProgress
  }> {
    return from(this.getStatus()).pipe(
      filter(Boolean),
      catchError(e => {
        this.errorToastService.present(e)
        return of(e)
      }),
      take(1),
    )
  }
}

function getDecimal(progress: T.Progress): number {
  if (progress === true) {
    return 1
  } else if (!progress || !progress.total) {
    return 0
  } else {
    return progress.total && progress.done / progress.total
  }
}

function getPhaseBytes(
  progress:
    | false
    | {
        done: number
        total: number | null
      },
): string {
  return progress === false
    ? 'unknown'
    : `${progress.done} of ${progress.total}`
}
