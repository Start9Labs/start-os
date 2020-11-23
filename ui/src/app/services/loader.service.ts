import { Injectable } from '@angular/core'
import { concatMap, finalize } from 'rxjs/operators'
import { Observable, from, Subject } from 'rxjs'
import { fromAsync$, fromAsyncP, emitAfter$, fromSync$ } from '../util/rxjs.util'
import { LoadingController } from '@ionic/angular'
import { LoadingOptions } from '@ionic/core'

@Injectable({
  providedIn: 'root',
})
export class LoaderService {
  private loadingOptions: LoadingOptions = defaultOptions()
  constructor (private readonly loadingCtrl: LoadingController) { }

  private loader: HTMLIonLoadingElement

  public get ionLoader (): HTMLIonLoadingElement {
    return this.loader
  }

  public get ctrl () {
    return this.loadingCtrl
  }

  private setOptions (l: LoadingOptions): LoaderService {
    this.loadingOptions = l
    return this
  }

  of (overrideOptions: LoadingOptions): LoaderService {
    return new LoaderService(this.loadingCtrl).setOptions(Object.assign(defaultOptions(), overrideOptions))
  }

  displayDuring$<T> (o: Observable<T>): Observable<T> {
    let shouldDisplay = true
    const displayIfItsBeenAtLeast = 10 // ms
    return fromAsync$(
      async () => {
        this.loader = await this.loadingCtrl.create(this.loadingOptions)
        emitAfter$(displayIfItsBeenAtLeast).subscribe(() => { if (shouldDisplay) this.loader.present() })
      },
    ).pipe(
      concatMap(() => o),
      finalize(() => {
        this.loader.dismiss(); shouldDisplay = false; this.loader = undefined
       }),
    )
  }

  displayDuringP<T> (p: Promise<T>): Promise<T> {
    return this.displayDuring$(from(p)).toPromise()
  }

  displayDuringAsync<T> (thunk: () => Promise<T>): Promise<T> {
    return this.displayDuringP(fromAsyncP(thunk))
  }
}

export function markAsLoadingDuring$<T> ($trigger$: Subject<boolean>, o: Observable<T>): Observable<T> {
  let shouldBeOn = true
  const displayIfItsBeenAtLeast = 5 // ms
  return fromSync$(() => {
    emitAfter$(displayIfItsBeenAtLeast).subscribe(() => { if (shouldBeOn) $trigger$.next(true) })
  }).pipe(
    concatMap(() => o),
    finalize(() => {
      $trigger$.next(false)
      shouldBeOn = false
    }),
 )
}

export function markAsLoadingDuringP<T> ($trigger$: Subject<boolean>, p: Promise<T>): Promise<T> {
  return markAsLoadingDuring$($trigger$, from(p)).toPromise()
}

export function markAsLoadingDuringAsync<T> ($trigger$: Subject<boolean>, thunk: () => Promise<T>): Promise<T> {
  return markAsLoadingDuringP($trigger$, fromAsyncP(thunk))
}


const defaultOptions: () => LoadingOptions = () => ({
  spinner: 'lines',
  cssClass: 'loader',
  backdropDismiss: true,
})
