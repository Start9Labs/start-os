import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiAlert, TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import {
  distinctUntilChanged,
  endWith,
  filter,
  merge,
  Observable,
  Subject,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  standalone: true,
  selector: 'update-toast',
  template: `
    <ng-template
      [tuiAlert]="!!(visible$ | async)"
      [tuiAlertOptions]="{
        label: 'StartOS download complete!',
        status: 'success',
        autoClose: 0
      }"
      (tuiAlertChange)="onDismiss()"
    >
      Restart your server for these updates to take effect. It can take several
      minutes to come back online.
      <div>
        <button
          tuiButton
          appearance="secondary"
          size="s"
          style="margin-top: 8px"
          (click)="restart()"
        >
          Restart
        </button>
      </div>
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiAlert, AsyncPipe],
})
export class UpdateToastComponent {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly dismiss$ = new Subject<boolean>()

  readonly visible$: Observable<boolean> = merge(
    this.dismiss$,
    inject(PatchDB<DataModel>)
      .watch$('serverInfo', 'statusInfo', 'updated')
      .pipe(distinctUntilChanged(), filter(Boolean), endWith(false)),
  )

  onDismiss() {
    this.dismiss$.next(false)
  }

  async restart(): Promise<void> {
    this.onDismiss()

    const loader = this.loader.open('Restarting...').subscribe()

    try {
      await this.api.restartServer({})
    } catch (e: any) {
      await this.errorService.handleError(e)
    } finally {
      await loader.unsubscribe()
    }
  }
}
