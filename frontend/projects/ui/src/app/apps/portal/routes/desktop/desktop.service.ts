import { inject, Injectable } from '@angular/core'
import { TuiAlertService } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { BehaviorSubject, first } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class DesktopService {
  private readonly alerts = inject(TuiAlertService)
  private readonly api = inject(ApiService)

  readonly desktop$ = new BehaviorSubject<readonly string[] | undefined>(
    undefined,
  )

  constructor() {
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('ui', 'desktop')
      .pipe(first())
      .subscribe(desktop => {
        if (!this.desktop$.value) {
          this.desktop$.next(desktop)
        }
      })
  }

  add(id: string) {
    this.desktop$.next(this.desktop$.value?.concat(id))
    this.save(this.desktop$.value)
  }

  remove(id: string) {
    this.desktop$.next(this.desktop$.value?.filter(x => x !== id))
    this.save(this.desktop$.value)
  }

  save(ids: readonly string[] = []) {
    this.api
      .setDbValue(['desktop'], ids)
      .catch(() =>
        this.alerts
          .open(
            'Desktop might be out of sync. Please refresh the page to fix it.',
            { status: 'warning' },
          )
          .subscribe(),
      )
  }
}
