import { Injectable } from '@angular/core'
import { EMPTY, Observable } from 'rxjs'
import { switchMap } from 'rxjs/operators'

import { PatchMonitorService } from './patch-monitor.service'
import { ConnectionService } from 'src/app/services/connection.service'

// Start connection monitor upon PatchDb start
@Injectable()
export class ConnectionMonitorService extends Observable<unknown> {
  private readonly stream$ = this.patchMonitor.pipe(
    switchMap(started => (started ? this.connectionService.start() : EMPTY)),
  )

  constructor(
    private readonly patchMonitor: PatchMonitorService,
    private readonly connectionService: ConnectionService,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
