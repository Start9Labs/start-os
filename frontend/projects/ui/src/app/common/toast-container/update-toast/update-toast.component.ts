import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { Observable, Subject, merge } from 'rxjs'

import { UpdateToastService } from './update-toast.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'update-toast',
  templateUrl: './update-toast.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class UpdateToastComponent {
  private readonly dismiss$ = new Subject<boolean>()

  readonly visible$: Observable<boolean> = merge(this.dismiss$, this.update$)

  constructor(
    @Inject(UpdateToastService) private readonly update$: Observable<boolean>,
    private readonly embassyApi: ApiService,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
  ) {}

  onDismiss() {
    this.dismiss$.next(false)
  }

  async restart(): Promise<void> {
    this.onDismiss()

    const loader = this.loader.open('Restarting...').subscribe()

    try {
      await this.embassyApi.restartServer({})
    } catch (e: any) {
      await this.errorService.handleError(e)
    } finally {
      await loader.unsubscribe()
    }
  }
}
