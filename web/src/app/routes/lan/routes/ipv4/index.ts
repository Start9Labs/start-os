import {
  ChangeDetectionStrategy,
  Component,
  computed,
  effect,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { WA_WINDOW } from '@ng-web-apis/common'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiNotificationService,
  tuiNumberFormatProvider,
  TuiTitle,
} from '@taiga-ui/core'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { catchError, EMPTY, firstValueFrom, startWith } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/components/form'
import { RECONNECTING_DIALOG } from 'src/app/components/reconnecting-dialog'
import { ApiService } from 'src/app/services/api/api.service'
import { provideFormService } from 'src/app/services/form.service'
import { NetworkRestartService } from 'src/app/services/network-restart.service'
import { LanIpv4Ip } from './form/ip'
import { LanIpv4Service } from './service'
import { LanIpv4Summary } from './summary'
import { buildRouterIp, getLanIpv4Form, LanIpv4Form } from './utils'

@Component({
  template: `
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article lanIpv4Summary [formLoading]="!service.data()"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!service.data()"
      (reset.prevent)="form.reset(service.data())"
      (ngSubmit)="onSave()"
    >
      <lan-ipv4-ip formGroupName="ip" />
      @if (service.data()) {
        <footer appFooter [blocked]="saveBlocked()"></footer>
      }
    </form>
  `,
  host: { class: 'g-page' },
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    Footer,
    Form,
    LanIpv4Summary,
    LanIpv4Ip,
  ],
  providers: [
    provideFormService(LanIpv4Service),
    tuiNumberFormatProvider({ precision: 0 }),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class LanIpv4 {
  protected readonly builder = inject(NonNullableFormBuilder)
  protected readonly service = inject(LanIpv4Service)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly alerts = inject(TuiNotificationService)
  private readonly networkRestart = inject(NetworkRestartService)
  private readonly window = inject(WA_WINDOW)

  readonly form = getLanIpv4Form(this.builder)
  readonly hasStaticIps = signal(false)

  constructor() {
    effect(() => {
      const data = this.service.data()
      if (data && this.form.pristine) {
        this.form.reset(data)
      }
    })
    this.loadDependencies()
  }

  private readonly formValue = toSignal(
    this.form.valueChanges.pipe(startWith(this.form.getRawValue())),
    { requireSync: true },
  )

  readonly saveBlocked = computed(() => {
    if (!this.hasStaticIps()) return null
    const data = this.service.data()
    if (!data) return null
    const current = this.formValue()
    if (
      current.ip?.firstOctet !== data.ip.firstOctet ||
      current.ip?.routerOctet !== data.ip.routerOctet
    ) {
      return 'Cannot change subnet while devices have static IP reservations'
    }
    return null
  })

  private async loadDependencies() {
    try {
      const devices = await this.api.devicesList()
      if (devices.some(d => d.ipv4_static)) {
        this.hasStaticIps.set(true)
      }
    } catch {
      // If we can't check, leave the form enabled — backend validates anyway
    }
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    const oldIp = this.service.data()
      ? buildRouterIp(this.service.data()!.ip)
      : ''
    const newIp = buildRouterIp(this.form.getRawValue().ip)
    const currentHost = this.window.location.hostname

    if (oldIp !== newIp) {
      // IP changing — call API directly so VPN errors throw here (no toast)
      try {
        await this.service.saveForIpChange(this.form.getRawValue())
      } catch (e: any) {
        if (!e?.message?.includes('VPN client')) {
          this.alerts
            .open(e?.message || 'Failed to save', { appearance: 'negative' })
            .subscribe()
          return
        }
        const confirmed = await firstValueFrom(
          this.dialogs.open(TUI_CONFIRM, {
            label: 'Inbound VPN Will Be Deleted',
            data: {
              content:
                'Changing the router IP will invalidate all existing VPN client configurations. The inbound VPN server and its peers will be removed and must be re-created.',
              yes: 'Delete VPN & Continue',
              no: 'Cancel',
            },
          }),
        ).catch(() => false)
        if (!confirmed) return
        try {
          await this.service.saveForIpChange(this.form.getRawValue(), true)
        } catch {
          return
        }
      }

      if (currentHost === oldIp) {
        this.dialogs
          .open(
            "Your router's IP address has changed. The UI is now available at the new address.",
            {
              label: 'IP Address Changed',
              dismissible: false,
              data: 'Open',
            },
          )
          .subscribe({
            complete: () => {
              this.window.location.href = `http://${newIp}`
            },
          })
      } else {
        await firstValueFrom(
          this.dialogs
            .open(RECONNECTING_DIALOG, {
              label: 'Reconnecting',
              closable: false,
              dismissible: false,
              data: 'Applying LAN settings...',
            })
            .pipe(catchError(() => EMPTY)),
        )
        this.networkRestart.recovered()
        this.alerts
          .open('LAN settings applied', { appearance: 'positive' })
          .subscribe()
      }
      return
    }

    const saved = await this.service.save(this.form.getRawValue())
    if (saved) this.form.markAsPristine()
  }
}
