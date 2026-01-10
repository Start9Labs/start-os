import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { tuiNumberFormatProvider, TuiTitle } from '@taiga-ui/core'
import { TuiDialogService } from '@taiga-ui/experimental'
import { TuiHeader } from '@taiga-ui/layout'
import { WA_WINDOW } from '@ng-web-apis/common'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { IPv4Aside } from './aside'
import { LanIpv4Ip } from './ip'
import { IpChangedDialog } from './ip-changed-dialog'
import { LanIpv4Service } from './service'
import { LanIpv4Summary } from './summary'
import { buildFullIp, getLanIpv4Form, LanIpv4Form } from './utils'

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
        <footer appFooter [disabled]="form.pristine"></footer>
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
  private readonly dialogs = inject(TuiDialogService)
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
      ? buildFullIp(this.service.data()!.ip)
      : ''
    const newIp = buildFullIp(this.form.getRawValue().ip)
    const currentHost = this.window.location.hostname

    const saved = await this.service.save(this.form.getRawValue())

    if (saved) {
      this.form.markAsPristine()

      // If IP changed and user is accessing via old IP, show redirect dialog
      if (oldIp !== newIp && currentHost === oldIp) {
        this.dialogs
          .open(new PolymorpheusComponent(IpChangedDialog), {
            label: 'IP Address Changed',
            closable: false,
            dismissible: false,
            size: 's',
            data: newIp,
          })
          .subscribe()
      }
    }
  }
}
