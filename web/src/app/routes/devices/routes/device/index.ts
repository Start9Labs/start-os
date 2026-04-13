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
  TuiError,
  TuiHintDirective,
  TuiInput,
  TuiLink,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiSkeleton, TuiSwitch } from '@taiga-ui/kit'
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
import { ApiService } from 'src/app/services/api/api.service'
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
      <section>
        <div>
          <tui-textfield>
            <label tuiLabel>Name</label>
            <input
              tuiInput
              formControlName="name"
              [placeholder]="data()?.hostname ?? ''"
            />
          </tui-textfield>
          <tui-error formControlName="name" />
        </div>
      </section>
      <section formGroupName="ip">
        <div>
          <tui-textfield>
            <label tuiLabel>IPv4 address</label>
            <input tuiInput formControlName="ipv4" [readOnly]="!ipv4Static()" />
          </tui-textfield>
          <tui-error formControlName="ipv4" />
        </div>
        <label tuiLabel>
          <input tuiSwitch type="checkbox" formControlName="ipv4Static" />
          Reserve
          <i tuiHint="Required by a published port rule"></i>
        </label>
        <div>
          <tui-textfield>
            <label tuiLabel>IPv6 address</label>
            <input tuiInput formControlName="ipv6" [readOnly]="!ipv6Static()" />
          </tui-textfield>
          <tui-error formControlName="ipv6" />
        </div>
        <label tuiLabel>
          <input tuiSwitch type="checkbox" formControlName="ipv6Static" />
          Reserve
          <i tuiHint="Required by a published port rule"></i>
        </label>
      </section>
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
    DeviceSummary,
    TuiSkeleton,
    TuiInput,
    TuiError,
    TuiHintDirective,
    TuiSwitch,
  ],
})
export default class DeviceDetail {
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)
  private readonly api = inject(ApiService)

  readonly service = inject(DevicesService)
  readonly mac = this.route.snapshot.queryParams['mac']
  readonly returnUrl = history.state?.['returnUrl'] || '/devices'
  readonly form = getDeviceForm(inject(NonNullableFormBuilder))
  readonly data = computed(
    () => this.service.data()?.find(d => d.mac === this.mac) ?? null,
  )

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
      updateDeviceValidators(this.form, this.ipv4Static(), this.ipv6Static())
    })
  }

  private async loadDependencies() {
    const ports = await this.api.publishedPortsList()
    const macUpper = this.mac.toUpperCase()
    const devicePorts = ports.filter(
      p => p.device_mac.toUpperCase() === macUpper && p.enabled,
    )
    if (devicePorts.some(p => p.ipv4)) {
      this.form.controls.ip.controls.ipv4Static.disable()
    }
    if (devicePorts.some(p => p.ipv6)) {
      this.form.controls.ip.controls.ipv6Static.disable()
    }
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

    if (success) this.form.markAsPristine()
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

  async onForget() {
    if (await this.service.forget(this.mac)) {
      this.router.navigate(['..'], { relativeTo: this.route })
    }
  }
}
