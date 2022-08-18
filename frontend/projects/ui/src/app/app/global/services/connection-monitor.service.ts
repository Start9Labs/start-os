import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'

// Start connection monitor upon app init
@Injectable()
export class ConnectionMonitorService extends Observable<unknown> {
  private readonly stream$ = this.connectionService.start()

  constructor(private readonly connectionService: ConnectionService) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
