import { Injectable } from '@angular/core'
import { Update } from 'patch-db-client'
import { ReplaySubject } from 'rxjs'
import { DataModel } from './data-model'

@Injectable({
  providedIn: 'root',
})
export class ResponseSyncService {
  readonly stream$ = new ReplaySubject<Update<DataModel>[]>(1)
}
