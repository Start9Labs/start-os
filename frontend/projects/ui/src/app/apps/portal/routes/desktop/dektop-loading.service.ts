import { inject, Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import {
  endWith,
  ignoreElements,
  Observable,
  shareReplay,
  startWith,
  take,
  tap,
} from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { DesktopService } from '../../services/desktop.service'

/**
 * This service loads initial values for desktop items
 * and is used to show loading indicator.
 */
@Injectable({
  providedIn: 'root',
})
export class DektopLoadingService extends Observable<boolean> {
  private readonly desktop = inject(DesktopService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly loading = this.patch.watch$('ui', 'desktop').pipe(
    take(1),
    tap(items => (this.desktop.items = items.filter(Boolean))),
    ignoreElements(),
    startWith(true),
    endWith(false),
    shareReplay(1),
  )

  constructor() {
    super(subscriber => this.loading.subscribe(subscriber))
  }
}
