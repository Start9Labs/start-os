import { inject, Injectable } from '@angular/core'
import { Observable, retry, shareReplay } from 'rxjs'
import { Metrics } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class MetricsService extends Observable<Metrics> {
  private readonly metrics$ = inject(ApiService)
    .openMetricsWebsocket$({
      url: '',
    })
    .pipe(retry(), shareReplay(1))

  constructor() {
    super(subscriber => this.metrics$.subscribe(subscriber))
  }
}
