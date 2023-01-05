import { Injectable } from '@angular/core'
import { Observable, Subject, merge, BehaviorSubject } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { ApiService } from '../../../services/api/embassy-api.service'
import { DataModel } from '../../../services/patch-db/data-model'
import { take } from 'rxjs/operators'

@Injectable({
  providedIn: 'root',
})
export class WidgetsService extends Observable<boolean> {
  private readonly manual$ = new BehaviorSubject(false)

  constructor(private readonly api: ApiService, patch: PatchDB<DataModel>) {
    super(subscriber => this.manual$.subscribe(subscriber))

    patch
      .watch$('ui', 'widgets', 'open')
      .pipe(take(1))
      .subscribe(value => {
        this.manual$.next(value)
      })
  }

  toggle(value: boolean) {
    this.manual$.next(value)
    this.api.setDbValue(['widgets', 'open'], value)
  }
}
