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
  readonly fullProgress$ = this.getRunningStatus$().pipe(
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

@Pipe({
  name: 'toDetails',
})
export class ToDetailsPipe implements PipeTransform {
  transform({ overall, phases }: T.FullProgress) {
    const currentPhase = phases.find(p => p.progress !== true) || {
      name: 'Total',
      progress: overall,
    }

    return {
      overallDecimal: getDecimal(overall),
      currentPhase: {
        name: currentPhase.name,
        decimal: getDecimal(currentPhase.progress),
      },
    }
  }
}

function getDecimal(progress: T.Progress): number {
  if (progress === true) {
    return 1
  } else if (!progress || !progress.total) {
    return 0
  } else {
    return progress.done / progress.total
  }
}
