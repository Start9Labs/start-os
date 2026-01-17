import {
  ChangeDetectionStrategy,
  Component,
  computed,
  effect,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { ActivatedRoute, Router, RouterLink } from '@angular/router'
import {
  TuiButton,
  TuiLink,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import { DevicesService } from 'src/app/routes/home/routes/devices/service'
import {
  DEVICE_VALIDATION_ERRORS,
  getDeviceForm,
  updateDeviceValidators,
} from 'src/app/routes/home/routes/devices/utils'

import { DeviceAside } from './aside'
import { DeviceIp } from './ip'
import { DeviceName } from './name'
import { DeviceSummary } from './summary'

@Component({
  template: `
    <device-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2>
          <a
            tuiLink
            routerLink=".."
            appearance=""
            iconStart="@tui.chevron-left"
            [style.font]="'inherit'"
            [style.text-decoration]="'none'"
          >
            {{ data()?.name || 'Device' }}
          </a>
        </h2>
      </hgroup>
    </header>
    <header tuiHeader="h6">
      <h2 tuiTitle>Summary</h2>
      <aside tuiAccessories>
        @if (data()?.status !== 'online') {
          <button
            tuiButton
            size="m"
            appearance="secondary"
            (click)="onForget()"
          >
            Forget
          </button>
        }
        @if (data()?.status === 'blocked') {
          <button
            tuiButton
            size="m"
            appearance="secondary-destructive"
            (click)="onUnblock()"
          >
            Unblock
          </button>
        }
        @if (data()?.status !== 'blocked') {
          <button
            tuiButton
            size="m"
            appearance="secondary-destructive"
            (click)="onBlock()"
          >
            Block
          </button>
        }
      </aside>
    </header>
    <article deviceSummary [formLoading]="!data()"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!data()"
      (reset.prevent)="onCancel()"
      (ngSubmit)="onSave()"
    >
      <device-name [formGroup]="form" [hostname]="data()?.hostname ?? ''" />
      <hr />
      <device-ip formGroupName="ip" />
      @if (data()) {
        <footer appFooter></footer>
      }
    </form>
  `,
  styles: `
    :host {
      padding-top: 0;
    }

    header[tuiHeader='h6'] {
      align-items: center;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  providers: [tuiValidationErrorsProvider(DEVICE_VALIDATION_ERRORS)],
  imports: [
    RouterLink,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiLink,
    TuiButton,
    Footer,
    Form,
    Help,
    DeviceAside,
    DeviceIp,
    DeviceName,
    DeviceSummary,
  ],
})
export default class DeviceDetail {
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)

  readonly service = inject(DevicesService)
  readonly mac = this.route.snapshot.params['mac']

  readonly form = getDeviceForm(inject(NonNullableFormBuilder))

  readonly data = computed(() => {
    const allDevices = this.service.data()
    return allDevices?.find(d => d.mac === this.mac) ?? null
  })

  readonly ipv4Static = toSignal(
    this.form.controls.ip.controls.ipv4Static.valueChanges.pipe(
      startWith(this.form.controls.ip.controls.ipv4Static.value),
    ),
    { requireSync: true },
  )

  readonly ipv6Static = toSignal(
    this.form.controls.ip.controls.ipv6Static.valueChanges.pipe(
      startWith(this.form.controls.ip.controls.ipv6Static.value),
    ),
    { requireSync: true },
  )

  constructor() {
    // Reset form when data loads
    effect(() => {
      const data = this.data()
      if (data && this.form.pristine) {
        this.form.reset({
          name: data.name,
          ip: {
            ipv4Static: data.ipv4Static,
            ipv4: data.ipv4 ?? '',
            ipv6Static: data.ipv6Static,
            ipv6: data.ipv6 ?? '',
          },
        })
        updateDeviceValidators(this.form, data.ipv4Static, data.ipv6Static)
      }
    })

    // Update validators when static toggles change
    effect(() => {
      updateDeviceValidators(this.form, this.ipv4Static(), this.ipv6Static())
    })
  }

  async onSave() {
    if (this.form.invalid) return

    const formValue = this.form.getRawValue()
    const success = await this.service.update(this.mac, {
      name: formValue.name,
      ipv4Static: formValue.ip.ipv4Static,
      ipv4: formValue.ip.ipv4,
      ipv6Static: formValue.ip.ipv6Static,
      ipv6: formValue.ip.ipv6,
    })
    if (success) {
      this.form.markAsPristine()
    }
  }

  onCancel() {
    const data = this.data()
    if (data) {
      this.form.reset({
        name: data.name,
        ip: {
          ipv4Static: data.ipv4Static,
          ipv4: data.ipv4 ?? '',
          ipv6Static: data.ipv6Static,
          ipv6: data.ipv6 ?? '',
        },
      })
    }
  }

  async onBlock() {
    const success = await this.service.block(this.mac)
    if (success) {
      this.router.navigate(['..'], { relativeTo: this.route })
    }
  }

  async onUnblock() {
    const success = await this.service.unblock(this.mac)
    if (success) {
      this.router.navigate(['..'], { relativeTo: this.route })
    }
  }

  async onForget() {
    const success = await this.service.forget(this.mac)
    if (success) {
      this.router.navigate(['..'], { relativeTo: this.route })
    }
  }
}
