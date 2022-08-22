import { Injectable } from '@angular/core'
import { endWith, Observable } from 'rxjs'
import { distinctUntilChanged, filter } from 'rxjs/operators'
import { PatchDbService } from '../../../services/patch-db/patch-db.service'

@Injectable({ providedIn: 'root' })
export class UpdateToastService extends Observable<boolean> {
  private readonly stream$ = this.patch
    .watch$('server-info', 'status-info', 'updated')
    .pipe(distinctUntilChanged(), filter(Boolean), endWith(false))

  constructor(private readonly patch: PatchDbService) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
