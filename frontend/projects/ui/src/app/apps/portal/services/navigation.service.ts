import { Injectable } from '@angular/core'
import { BehaviorSubject, Observable } from 'rxjs'
import { NavigationItem } from '../types/navigation-item'

@Injectable({
  providedIn: 'root',
})
export class NavigationService {
  private readonly tabs = new BehaviorSubject<readonly NavigationItem[]>([])

  getTabs(): Observable<readonly NavigationItem[]> {
    return this.tabs
  }

  removeTab({ routerLink }: NavigationItem) {
    this.tabs.next(this.tabs.value.filter(t => t.routerLink !== routerLink))
  }

  addTab(tab: NavigationItem) {
    if (this.tabs.value.every(t => t.routerLink !== tab.routerLink)) {
      this.tabs.next([...this.tabs.value, tab])
    }
  }

  hasTab(path: string): boolean {
    return this.tabs.value.some(t => t.routerLink === path)
  }
}
