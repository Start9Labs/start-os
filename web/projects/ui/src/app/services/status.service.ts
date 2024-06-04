import { Injectable } from '@angular/core'
import { Observable, defer, retry } from 'rxjs'
import { ApiService } from '../services/api/embassy-api.service'
import { RR } from './api/api.types'

@Injectable({
  providedIn: 'root',
})
export class StateService extends Observable<RR.ServerState> {
  private readonly stream$ = defer(() => this.api.getState()).pipe(
    retry({
      delay: 2000,
    }),
  )

  constructor(private readonly api: ApiService) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
