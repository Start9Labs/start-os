import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { AbstractPkgImplementationService } from '@start9labs/marketplace'

@Injectable({
  providedIn: 'root',
})
export class PkgImplementationService extends AbstractPkgImplementationService {
  getAltStatus$(): Observable<boolean> {
    return this.altImplActive$
  }

  toggleAltStatus(status: boolean) {
    this.altImplActive$.next(status)
  }
}
