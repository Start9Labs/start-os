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

  removeTab(routerLink: string) {
    this.tabs.next(
      this.tabs.value.filter(t => !t.routerLink.startsWith(routerLink)),
    )
  }

  addTab(tab: NavigationItem) {
    const current = this.tabs.value.find(t =>
      t.routerLink.startsWith(tab.routerLink),
    )

    this.tabs.next(
      current
        ? this.tabs.value.map(t => (t === current ? tab : t))
        : this.tabs.value.concat(tab),
    )
  }

  updateTab(old: string, routerLink: string) {
    this.tabs.next(
      this.tabs.value.map(t =>
        t.routerLink === old ? { ...t, routerLink } : t,
      ),
    )
  }

  hasTab(path: string): boolean {
    return this.tabs.value.some(t => t.routerLink === path)
  }

  hasSubtab(path: string): boolean {
    return this.tabs.value.some(t => t.routerLink.startsWith(path))
  }
}
