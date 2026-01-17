import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { WA_WINDOW } from '@ng-web-apis/common'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { tuiNumberFormatProvider, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { IPv4Aside } from './aside'
import { LanIpv4Ip } from './form/ip'
import { LanIpv4Service } from './service'
import { LanIpv4Summary } from './summary'
import { buildRouterIp, getLanIpv4Form, LanIpv4Form } from './utils'

@Component({
  template: `
    <ipv4-aside *help />
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
        <footer appFooter></footer>
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
    Help,
    LanIpv4Summary,
    IPv4Aside,
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
  protected readonly service = injectFormService<LanIpv4Form>()
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly window = inject(WA_WINDOW)

  readonly form = getLanIpv4Form(this.builder)

  constructor() {
    effect(() => {
      const data = this.service.data()
      if (data && this.form.pristine) {
        this.form.reset(data)
      }
    })
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
    const saved = await this.service.save(this.form.getRawValue())

    if (saved) {
      this.form.markAsPristine()

      // If IP changed and user is accessing via old IP, show redirect dialog
      if (oldIp !== newIp && currentHost === oldIp) {
        this.dialogs
          .open(
            "Your router's IP address has changed. The UI is now available at the new address.",
            {
              label: 'IP Address Changed',
              dismissible: false,
              size: 's',
              data: 'Open',
            },
          )
          .subscribe({
            complete: () => {
              this.window.location.href = `http://${newIp}`
            },
          })
      }
    }
  }
}
