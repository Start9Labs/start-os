import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { LoadingController } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'
import { Observable, Subject, merge } from 'rxjs'

import { UpdateToastService } from './update-toast.service'
import { ApiService } from '../../../services/api/embassy-api.service'

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
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
  ) {}

  onDismiss() {
    this.dismiss$.next(false)
  }

  async restart(): Promise<void> {
    this.onDismiss()

    const loader = await this.loadingCtrl.create({
      message: 'Restarting...',
    })

    await loader.present()

    try {
      await this.embassyApi.restartServer({})
    } catch (e: any) {
      await this.errToast.present(e)
    } finally {
      await loader.dismiss()
    }
  }
}
