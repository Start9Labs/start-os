import { Injectable } from '@angular/core'
import { endWith, Observable } from 'rxjs'
import { distinctUntilChanged, filter } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Injectable({ providedIn: 'root' })
export class UpdateToastService extends Observable<boolean> {
  private readonly stream$ = this.patch
    .watch$('server-info', 'status-info', 'updated')
    .pipe(distinctUntilChanged(), filter(Boolean), endWith(false))

  constructor(private readonly patch: PatchDB<DataModel>) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
