import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import { ErrorService, LoadingService, pauseFor } from '@start9labs/shared'
import {
  TuiAlertService,
  TuiAppearance,
  TuiButton,
  TuiDialogOptions,
  TuiLink,
  TuiLoader,
  TuiNotification,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { catchError, defer, map, merge, Observable, of, Subject } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { WifiTableComponent } from './table.component'
import { parseWifi, WifiData, WiFiForm } from './utils'
import { wifiSpec } from './wifi.const'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      WiFi
    </ng-container>
    <header tuiHeader>
      <tui-notification appearance="negative">
        <div tuiTitle>
          Deprecated
          <div tuiSubtitle>
            WiFi support will be removed in StartOS v0.4.1. If you do not have
            access to Ethernet, you can use a WiFi extender to connect to the
            local network, then connect your server to the extender via
            Ethernet. Please contact Start9 support with any questions or
            concerns.
          </div>
        </div>
      </tui-notification>
    </header>
    @if (status()?.interface) {
      <section class="g-card">
        <header>
          Wi-Fi
          <input
            type="checkbox"
            tuiSwitch
            [style.margin-inline-start]="'auto'"
            [ngModel]="status()?.enabled"
            (ngModelChange)="onToggle($event)"
          />
        </header>
        @if (status()?.enabled) {
          @if (wifi(); as data) {
            @if (data.known.length) {
              <p class="g-secondary">KNOWN NETWORKS</p>
              <div
                tuiCardLarge="compact"
                tuiAppearance="neutral"
                [wifi]="data.known"
              ></div>
            }
            @if (data.available.length) {
              <p class="g-secondary">OTHER NETWORKS</p>
              <div
                tuiCardLarge="compact"
                tuiAppearance="neutral"
                [wifi]="data.available"
              ></div>
            }
            <p>
              <button tuiButton (click)="other(data)">Add</button>
            </p>
          } @else {
            <tui-loader [style.height.rem]="5" />
          }
        } @else {
          <app-placeholder icon="@tui.wifi">WiFi is disabled</app-placeholder>
        }
      </section>
    } @else {
      <app-placeholder icon="@tui.wifi">
        No wireless interface detected
      </app-placeholder>
    }
  `,
  styles: `
    :host {
      max-width: 40rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    FormsModule,
    TuiButton,
    TuiSwitch,
    TuiCardLarge,
    TuiLoader,
    TuiAppearance,
    WifiTableComponent,
    TitleDirective,
    RouterLink,
    PlaceholderComponent,
    TuiHeader,
    TuiTitle,
    TuiLink,
    TuiNotification,
  ],
})
export default class SystemWifiComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly alerts = inject(TuiAlertService)
  private readonly update$ = new Subject<WifiData>()
  private readonly formDialog = inject(FormDialogService)
  private readonly cdr = inject(ChangeDetectorRef)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly status = toSignal(this.patch.watch$('serverInfo', 'network', 'wifi'))
  readonly wifi = toSignal(merge(this.getWifi$(), this.update$))

  async onToggle(enable: boolean) {
    const loader = this.loader
      .open(enable ? 'Enabling Wifi' : 'Disabling WiFi')
      .subscribe()

    try {
      await this.api.enableWifi({ enable })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  other(wifi: WifiData) {
    const options: Partial<TuiDialogOptions<FormContext<WiFiForm>>> = {
      label: wifiSpec.name,
      data: {
        spec: wifiSpec.spec,
        buttons: [
          {
            text: 'Save for Later',
            handler: async ({ ssid, password }) =>
              this.save(ssid, password, wifi),
          },
          {
            text: 'Save and Connect',
            handler: async ({ ssid, password }) =>
              this.saveAndConnect(ssid, password),
          },
        ],
      },
    }

    this.formDialog.open(FormComponent, options)
  }

  async saveAndConnect(ssid: string, password?: string): Promise<boolean> {
    const loader = this.loader
      .open('Connecting. This could take a while...')
      .subscribe()

    try {
      if (password) {
        await this.api.addWifi({
          ssid,
          password,
          priority: 0,
          connect: true,
        })
      } else {
        await this.api.connectWifi({ ssid })
      }

      await this.confirmWifi(ssid)
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async confirmWifi(ssid: string): Promise<void> {
    const maxAttempts = 5
    let attempts = 0

    while (true) {
      if (attempts > maxAttempts) {
        this.alerts
          .open('Check credentials and try again', {
            label: 'Failed to connect',
            appearance: 'warning',
          })
          .subscribe()
        break
      }

      try {
        const start = new Date().valueOf()
        const newWifi = await this.api.getWifi({}, 10000)
        const end = new Date().valueOf()
        if (newWifi.connected === ssid) {
          this.update$.next(parseWifi(newWifi))
          this.alerts
            .open('Connection successful!', { appearance: 'positive' })
            .subscribe()
          break
        } else {
          attempts++
          const diff = end - start
          // depending on the response time, wait a min of 1000 ms, and a max of 4000 ms in between retries.  Both 1000 and 4000 are arbitrary
          await pauseFor(Math.max(1000, 4000 - diff))
        }
      } catch (e) {
        attempts++
        console.warn(e)
      }
    }
  }

  private getWifi$(): Observable<WifiData> {
    return defer(() => this.api.getWifi({}, 10000)).pipe(
      map(res => parseWifi(res)),
      catchError((e: any) => {
        this.errorService.handleError(e)
        return of({ known: [], available: [] })
      }),
    )
  }

  private async save(
    ssid: string,
    password: string,
    wifi: WifiData,
  ): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.addWifi({
        ssid,
        password,
        priority: 0,
        connect: false,
      })
      wifi.known = wifi.known.concat({
        ssid,
        strength: 0,
        security: [],
        connected: false,
      })
      this.cdr.markForCheck()

      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}
