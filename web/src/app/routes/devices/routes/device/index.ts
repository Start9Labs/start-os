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
import { ActivatedRoute, Router, RouterLink } from '@angular/router'
import {
  TuiButton,
  TuiLink,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/components/form'
import { DevicesService } from 'src/app/routes/devices/service'
import {
  DEVICE_VALIDATION_ERRORS,
  getDeviceForm,
  updateDeviceValidators,
} from 'src/app/routes/devices/utils'
import { PublishedPortsUciService } from 'src/app/routes/published-ports/uci/service'
import { DeviceIp } from './ip'
import { DeviceName } from './name'
import { DeviceSummary } from './summary'

@Component({
  template: `
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2>
          <a
            tuiLink
            [routerLink]="returnUrl"
            appearance=""
            iconStart="@tui.chevron-left"
            [tuiSkeleton]="!data()"
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
        @if (data() && data()?.status !== 'online') {
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
        @if (data() && data()?.status !== 'blocked') {
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
      <device-ip formGroupName="ip" [ipv4Locked]="portUsage().usesIpv4" />
      @if (data()) {
        <footer appFooter></footer>
      }
    </form>
  `,
  styles: `
    header[tuiHeader='h6'] {
      align-items: center;
      max-width: 50rem;
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
    DeviceIp,
    DeviceName,
    DeviceSummary,
    TuiSkeleton,
  ],
})
export default class DeviceDetail {
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)
  private readonly publishedPortsUci = inject(PublishedPortsUciService)

  readonly service = inject(DevicesService)
  readonly mac = this.route.snapshot.queryParams['mac']
  readonly returnUrl = history.state?.['returnUrl'] || '/devices'

  readonly form = getDeviceForm(inject(NonNullableFormBuilder))

  // Track if this device has published ports using IPv4
  readonly portUsage = signal<{ usesIpv4: boolean; usesIpv6: boolean }>({
    usesIpv4: false,
    usesIpv6: false,
  })

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

  constructor() {
    // Refresh device data to get latest info
    this.service.refresh()

    // Load published port usage for this device
    this.loadDependencies()

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
      updateDeviceValidators(this.form, this.ipv4Static(), false)
    })
  }

  private async loadDependencies() {
    const usage = await this.publishedPortsUci.getDevicePortUsage(this.mac)
    this.portUsage.set(usage)
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
