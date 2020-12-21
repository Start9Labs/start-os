import { OnDestroy } from '@angular/core'
import { Subscription } from 'rxjs'

import { Constructor, ExtensionBase } from "./base.extension";

export function Cleanup<TBase extends Constructor>(Base: TBase) {
  return class C extends Base implements OnDestroy {
    private toCleanup: Subscription[] = []

    ngOnDestroy () {
      this.toCleanup.forEach(s => s.unsubscribe())
    }

    cleanup (...s: Subscription[]) {
      this.toCleanup.push(...s)
    }
  }
}

export const CleanupE = Cleanup(ExtensionBase)