import { Injectable } from '@angular/core'
import { BehaviorSubject, Observable } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export class SidebarService {
  openMap: Record<string, BehaviorSubject<boolean>> = {}

  setMap(ids: string[]) {
    ids.map(i => (this.openMap[i] = new BehaviorSubject(false)))
  }

  getToggleState(pkgId: string): Observable<boolean> {
    return this.openMap[pkgId]
  }

  toggleState(pkgId: string, open: boolean) {
    this.openMap[pkgId].next(open)
  }
}
