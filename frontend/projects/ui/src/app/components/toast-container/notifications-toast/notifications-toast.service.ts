import { Injectable } from '@angular/core'
import { endWith, Observable } from 'rxjs'
import { map, pairwise } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Injectable({ providedIn: 'root' })
export class NotificationsToastService extends Observable<boolean> {
  private readonly stream$ = this.patch
    .watch$('server-info', 'unread-notification-count')
    .pipe(
      pairwise(),
      map(([prev, cur]) => cur > prev),
      endWith(false),
    )

  constructor(private readonly patch: PatchDB<DataModel>) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
