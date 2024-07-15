import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { SwUpdate } from '@angular/service-worker'
import { Emver, LoadingService } from '@start9labs/shared'
import { TuiAutoFocus } from '@taiga-ui/cdk'
import { TuiButton, TuiDialog } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { debounceTime, endWith, map, merge, Subject } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  standalone: true,
  selector: 'refresh-alert',
  template: `
    <!--    <ng-template-->
    <!--      [tuiDialog]="show$ | async"-->
    <!--      [tuiDialogOptions]="{ label: 'Refresh Needed', size: 's' }"-->
    <!--      (tuiDialogChange)="onDismiss()"-->
    <!--    >-->
    <!--      Your user interface is cached and out of date. Hard refresh the page to-->
    <!--      get the latest UI.-->
    <!--      <ul>-->
    <!--        <li>-->
    <!--          <b>On Mac</b>-->
    <!--          : cmd + shift + R-->
    <!--        </li>-->
    <!--        <li>-->
    <!--          <b>On Linux/Windows</b>-->
    <!--          : ctrl + shift + R-->
    <!--        </li>-->
    <!--      </ul>-->
    <!--      <button-->
    <!--        tuiButton-->
    <!--        tuiAutoFocus-->
    <!--        appearance="secondary"-->
    <!--        style="float: right"-->
    <!--        (click)="onDismiss()"-->
    <!--      >-->
    <!--        Ok-->
    <!--      </button>-->
    <!--    </ng-template>-->
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiDialog, AsyncPipe, TuiButton, TuiAutoFocus],
})
export class RefreshAlertComponent {
  private readonly updates = inject(SwUpdate)
  private readonly loader = inject(LoadingService)
  private readonly emver = inject(Emver)
  private readonly config = inject(ConfigService)
  private readonly dismiss$ = new Subject<boolean>()

  readonly show$ = merge(
    this.dismiss$,
    inject(PatchDB<DataModel>)
      .watch$('serverInfo', 'version')
      .pipe(
        map(version => !!this.emver.compare(this.config.version, version)),
        endWith(false),
      ),
  ).pipe(debounceTime(0))

  // @TODO use this like we did on 0344
  onPwa = false

  ngOnInit() {
    this.onPwa = window.matchMedia('(display-mode: standalone)').matches
  }

  async pwaReload() {
    const loader = this.loader.open('Reloading PWA...').subscribe()

    try {
      // attempt to update to the latest client version available
      await this.updates.activateUpdate()
    } catch (e) {
      console.error('Error activating update from service worker: ', e)
    } finally {
      loader.unsubscribe()
      // always reload, as this resolves most out of sync cases
      window.location.reload()
    }
  }

  onDismiss() {
    this.dismiss$.next(false)
  }
}
