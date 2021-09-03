import { BehaviorSubject } from 'rxjs'
import { Injectable } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export class SplitPaneTracker {
  sidebarOpen$: BehaviorSubject<boolean> = new BehaviorSubject(false)
}