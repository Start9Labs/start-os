import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { SwUpdate } from '@angular/service-worker'
import { WA_WINDOW } from '@ng-web-apis/common'
import { LoadingService } from '@start9labs/shared'
import { Version } from '@start9labs/start-sdk'
import { TuiResponsiveDialog } from '@taiga-ui/addon-mobile'
import { TuiAutoFocus } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { distinctUntilChanged, map, merge, Subject } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'refresh-alert',
  template: `
    <ng-template
      [tuiResponsiveDialog]="show()"
      [tuiResponsiveDialogOptions]="{ label: 'Refresh Needed', size: 's' }"
      (tuiResponsiveDialogChange)="dismiss$.next()"
    >
      @if (isPwa) {
        <p>
          Your user interface is cached and out of date. Attempt to reload the
          PWA using the button below. If you continue to see this message,
          uninstall and reinstall the PWA.
        </p>
        <button
          tuiButton
          tuiAutoFocus
          appearance="secondary"
          style="float: right"
          [tuiAppearanceFocus]="false"
          (click)="pwaReload()"
        >
          Reload
        </button>
      } @else {
        Your user interface is cached and out of date. Hard refresh the page to
        get the latest UI.
        <ul>
          <li>
            <b>On Mac</b>
            : cmd + shift + R
          </li>
          <li>
            <b>On Linux/Windows</b>
            : ctrl + shift + R
          </li>
        </ul>
        <button
          tuiButton
          tuiAutoFocus
          appearance="secondary"
          style="float: right"
          [tuiAppearanceFocus]="false"
          (click)="dismiss$.next()"
        >
          Ok
        </button>
      }
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiResponsiveDialog, TuiButton, TuiAutoFocus],
})
export class RefreshAlertComponent {
  private readonly win = inject(WA_WINDOW)
  private readonly updates = inject(SwUpdate)
  private readonly loader = inject(LoadingService)
  private readonly version = Version.parse(inject(ConfigService).version)

  readonly dismiss$ = new Subject<void>()
  readonly isPwa = this.win.matchMedia('(display-mode: standalone)').matches

  readonly show = toSignal(
    merge(
      this.dismiss$.pipe(map(() => false)),
      inject<PatchDB<DataModel>>(PatchDB)
        .watch$('serverInfo', 'version')
        .pipe(
          distinctUntilChanged(),
          map(v => this.version.compare(Version.parse(v)) !== 'equal'),
        ),
    ),
    {
      initialValue: false,
    },
  )

  async pwaReload() {
    const loader = this.loader.open('Reloading PWA').subscribe()

    try {
      // attempt to update to the latest client version available
      await this.updates.activateUpdate()
    } catch (e) {
      console.error('Error activating update from service worker: ', e)
    } finally {
      loader.unsubscribe()
      // always reload, as this resolves most out of sync cases
      this.win.location.reload()
    }
  }
}
