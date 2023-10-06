import { Injectable } from '@angular/core'
import { BehaviorSubject, Observable } from 'rxjs'
import { NavigationItem } from '../types/navigation-item'

@Injectable({
  providedIn: 'root',
})
export class NavigationService {
  readonly tabs = new BehaviorSubject<readonly NavigationItem[]>([])

  getTabs(): Observable<readonly NavigationItem[]> {
    return this.tabs
  }

  removeTab(tab: NavigationItem) {
    this.tabs.next(this.tabs.value.filter(t => t !== tab))
  }

  addTab(tab: NavigationItem) {
    if (this.tabs.value.every(t => t.routerLink !== tab.routerLink)) {
      this.tabs.next([...this.tabs.value, tab])
    }
  }
}
