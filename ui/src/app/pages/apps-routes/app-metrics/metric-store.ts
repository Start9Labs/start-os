import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { AppMetrics } from '../../../util/metrics.util'

@Injectable({
  providedIn: 'root',
})
export class AppMetricStore {
  $metrics$: BehaviorSubject<AppMetrics> = new BehaviorSubject({ })
  watch () { return this.$metrics$.asObservable() }

  update (metrics: AppMetrics): void {
    this.$metrics$.next(metrics)
  }
}