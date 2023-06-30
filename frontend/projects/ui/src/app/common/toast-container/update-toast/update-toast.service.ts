import { Injectable } from '@angular/core'
import {
  distinctUntilChanged,
  filter,
  endWith,
  Observable,
  startWith,
  delay,
} from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Injectable({ providedIn: 'root' })
export class UpdateToastService extends Observable<boolean> {
  private readonly stream$ = this.patch
    .watch$('server-info', 'status-info', 'updated')
    .pipe(
      distinctUntilChanged(),
      filter(Boolean),
      endWith(false),
      startWith(true),
      delay(1000),
    )

  constructor(private readonly patch: PatchDB<DataModel>) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
