import { Injectable } from '@angular/core'
import { PatchDbService } from './patch-db/patch-db.service'
import { combineLatest, filter, map, Observable } from 'rxjs'

export interface ServerNameInfo {
  current: string
  default: string
}

@Injectable({ providedIn: 'root' })
export class ServerNameService {
  private readonly chosenName$ = this.patch.watch$('ui', 'name')
  private readonly lanName$ = this.patch
    .watch$('server-info', 'hostname')
    .pipe(filter(Boolean))

  readonly name$: Observable<ServerNameInfo> = combineLatest([
    this.chosenName$,
    this.lanName$,
  ]).pipe(
    map(([chosen, lan]) => {
      return {
        current: chosen || lan,
        default: lan,
      }
    }),
  )

  constructor(private readonly patch: PatchDbService) {}
}
