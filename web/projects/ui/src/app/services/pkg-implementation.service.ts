import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { AbstractPkgFlavorService } from '@start9labs/marketplace'

@Injectable({
  providedIn: 'root',
})
export class PkgFlavorService extends AbstractPkgFlavorService {
  getFlavorStatus$(): Observable<boolean> {
    return this.flavorActive
  }

  toggleFlavorStatus(status: boolean) {
    this.flavorActive.next(status)
  }
}
