import { BehaviorSubject } from 'rxjs'
import { Injectable } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export class SplitPaneTracker {
  readonly sidebarOpen$ = new BehaviorSubject<boolean>(false)
}
