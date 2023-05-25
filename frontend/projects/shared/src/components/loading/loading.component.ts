import { Component, inject, Input, Output } from '@angular/core'
import { delay, filter } from 'rxjs'
import { SetupService } from '../../services/setup.service'

@Component({
  selector: 'app-loading',
  templateUrl: 'loading.component.html',
  styleUrls: ['loading.component.scss'],
})
export class LoadingComponent {
  readonly progress$ = inject(SetupService)

  @Input()
  setupType?: 'fresh' | 'restore' | 'attach' | 'transfer'

  @Output()
  readonly finished = this.progress$.pipe(
    filter(progress => progress === 1),
    delay(500),
  )

  getMessage(progress: number | null): string {
    if (['fresh', 'attach'].includes(this.setupType || '')) {
      return 'Setting up your server'
    }

    if (!progress) {
      return 'Preparing data. This can take a while'
    } else if (progress < 1) {
      return 'Copying data'
    } else {
      return 'Finalizing'
    }
  }
}
