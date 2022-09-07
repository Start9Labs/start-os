import { Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { combineLatest, filter, map, Observable } from 'rxjs'
import { DataModel } from './patch-db/data-model'

export interface ServerNameInfo {
  current: string
  default: string
}

@Injectable({ providedIn: 'root' })
export class ServerNameService {
  private readonly chosenName$ = this.patch.watch$('ui', 'name')
  private readonly hostname$ = this.patch
    .watch$('server-info', 'hostname')
    .pipe(filter(Boolean))

  readonly name$: Observable<ServerNameInfo> = combineLatest([
    this.chosenName$,
    this.hostname$,
  ]).pipe(
    map(([chosen, hostname]) => {
      return {
        current: chosen || hostname,
        default: hostname,
      }
    }),
  )

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
