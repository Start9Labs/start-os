import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnInit,
  signal,
} from '@angular/core'
import { NonNullableFormBuilder } from '@angular/forms'
import { TuiAnimated, tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'
import { Help } from 'src/app/directives/help.directive'

import { IPv4Aside } from './aside'
import { Ipv4Dns } from './dns'
import { Ipv4Ip } from './ip'
import { Ipv4Service } from './service'
import { Ipv4Summary } from './summary'
import { DnsMode, IpMode } from './types'

@Component({
  template: `
    <ipv4-aside *help />
    <article ipv4Summary [formLoading]="loading()"></article>
    <form ipv4Ip [formLoading]="loading()"></form>
    <form ipv4Dns [formLoading]="loading()"></form>
    @if (!loading()) {
      <footer tuiAnimated class="g-footer">
        <button
          tuiButton
          appearance="flat"
          [disabled]="form.pristine"
          (click)="onReset()"
        >
          Cancel
        </button>
        <button
          tuiButton
          [disabled]="form.pristine || form.invalid"
          (click)="onSave()"
        >
          Save
        </button>
      </footer>
    }
  `,
  imports: [
    TuiButton,
    TuiAnimated,
    Help,
    Ipv4Summary,
    Ipv4Ip,
    Ipv4Dns,
    IPv4Aside,
  ],
  providers: [Ipv4Service],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ipv4 implements OnInit {
  private readonly builder = inject(NonNullableFormBuilder)
  private readonly service = inject(Ipv4Service)

  readonly loading = signal(true)
  readonly form = this.builder.group({
    ip: this.builder.group({
      mode: 'dhcp' as IpMode,
      dhcp: this.builder.group({
        wan: '',
        prefix: '',
        mask: '',
        gateway: '',
      }),
      static: this.builder.group({
        wan: '',
        prefix: '',
        mask: '',
        gateway: '',
      }),
      pppoe: this.builder.group({
        wan: '',
        password: '',
        vlan: '',
      }),
    }),
    dns: this.builder.group({
      mode: 'isp' as DnsMode,
      isp: this.builder.group({
        server: '',
      }),
      tls: this.builder.group({
        server: '',
      }),
      custom: this.builder.group({
        server: '',
        1: '',
        2: '',
        tls1: false,
        tls2: false,
      }),
      proxy: false,
    }),
  })

  public get ip() {
    return this.form.controls.ip.controls.mode.value
  }

  public get dns() {
    return this.form.controls.dns.controls.mode.value
  }

  async ngOnInit() {
    this.form.reset(await this.service.load())
    this.loading.set(false)
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
    } else if (await this.service.save(this.form.getRawValue())) {
      this.form.markAsPristine()
    }
  }

  onReset(): void {
    this.form.reset(this.service.reset())
  }
}
