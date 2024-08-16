import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  inject,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import {
  ErrorService,
  LoadingService,
  pauseFor,
  SharedPipesModule,
} from '@start9labs/shared'
import { TuiLet } from '@taiga-ui/cdk'
import {
  TuiAlertService,
  TuiButton,
  TuiDialogOptions,
  TuiLoader,
} from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { catchError, defer, map, merge, Observable, of, Subject } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { WifiInfoComponent } from './info.component'
import { WifiTableComponent } from './table.component'
import { parseWifi, WifiData, WiFiForm } from './utils'
import { wifiSpec } from './wifi.const'

@Component({
  template: `
    <wifi-info />
    <ng-container *tuiLet="enabled$ | async as enabled">
      <h3 class="g-title">
        Wi-Fi
        <input
          type="checkbox"
          tuiSwitch
          [ngModel]="enabled"
          (ngModelChange)="onToggle($event)"
        />
      </h3>

      <ng-container *ngIf="enabled">
        <ng-container *ngIf="wifi$ | async as wifi; else loading">
          <ng-container *ngIf="wifi.known.length">
            <h3 class="g-title">Known Networks</h3>
            <div tuiCard="l" [wifi]="wifi.known"></div>
          </ng-container>
          <ng-container *ngIf="wifi.available.length">
            <h3 class="g-title">Other Networks</h3>
            <div tuiCard="l" [wifi]="wifi.available"></div>
          </ng-container>
          <p>
            <button
              tuiButton
              size="s"
              appearance="opposite"
              (click)="other(wifi)"
            >
              Other...
            </button>
          </p>
        </ng-container>
        <ng-template #loading><tui-loader></tui-loader></ng-template>
      </ng-container>
    </ng-container>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiButton,
    TuiSwitch,
    TuiLet,
    TuiCardLarge,
    TuiLoader,
    SharedPipesModule,
    WifiInfoComponent,
    WifiTableComponent,
  ],
})
export class SettingsWifiComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly alerts = inject(TuiAlertService)
  private readonly update$ = new Subject<WifiData>()
  private readonly formDialog = inject(FormDialogService)
  private readonly cdr = inject(ChangeDetectorRef)

  readonly wifi$ = merge(this.getWifi$(), this.update$)
  readonly enabled$ = inject<PatchDB<DataModel>>(PatchDB).watch$(
    'serverInfo',
    'network',
    'wifi',
    'enabled',
  )

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
            status: 'warning',
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
            .open('Connection successful!', { status: 'success' })
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
