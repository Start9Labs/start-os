import { Injectable, OnDestroy } from '@angular/core'
import { Subscription } from 'rxjs'

@Injectable()
export abstract class Cleanup implements OnDestroy {
  private toCleanup: Subscription[] = []

  ngOnDestroy () {
    this.toCleanup.forEach(s => s.unsubscribe())
  }

  cleanup (...s: Subscription[]) {
    this.toCleanup.push(...s)
  }
}
