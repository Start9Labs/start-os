import {
  ChangeDetectionStrategy,
  Component,
  computed,
  effect,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiSelect,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import {
  injectFormService,
  provideFormService,
} from 'src/app/services/form.service'
import { DdnsAside } from './aside'
import { DdnsFields } from './fields'
import { DdnsService } from './service'
import { DdnsSummary } from './summary'
import {
  DdnsForm,
  DdnsProvider,
  DDNS_PROVIDER_LIST,
  DDNS_PROVIDERS,
  getDdnsForm,
  getProviderFields,
  updateDdnsValidators,
} from './utils'

@Component({
  template: `
    <ddns-aside *help />
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article ddnsSummary [formLoading]="!service.data()"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!service.data()"
      (reset.prevent)="form.reset(service.data())"
      (ngSubmit)="onSave()"
    >
      <section>
        <label tuiLabel>
          <input type="checkbox" tuiSwitch formControlName="enabled" />
          Enable Dynamic DNS
        </label>
      </section>
      @if (enabled()) {
        <section>
          <tui-textfield tuiChevron [tuiTextfieldCleaner]="false">
            <label tuiLabel>Provider</label>
            <input tuiSelect formControlName="provider" />
            <tui-data-list-wrapper
              *tuiTextfieldDropdown
              new
              [itemContent]="providerContent"
              [items]="providerList"
            />
          </tui-textfield>
          <ng-template #providerContent let-item>
            {{ getProviderLabel(item) }}
          </ng-template>
        </section>
        @if (providerFields().length) {
          <ddns-fields formGroupName="fields" [fields]="providerFields()" />
        }
      }
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
    TuiTextfield,
    TuiSwitch,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    Form,
    Footer,
    Help,
    DdnsSummary,
    DdnsFields,
    DdnsAside,
  ],
  providers: [provideFormService(DdnsService)],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ddns {
  protected readonly builder = inject(NonNullableFormBuilder)
  protected readonly service = injectFormService<DdnsForm>()

  readonly form = getDdnsForm(this.builder)

  protected readonly providerList = DDNS_PROVIDER_LIST

  getProviderLabel(provider: string): string {
    return DDNS_PROVIDERS[provider as DdnsProvider]?.label ?? provider
  }

  readonly enabled = toSignal(
    this.form.controls.enabled.valueChanges.pipe(
      startWith(this.form.controls.enabled.value),
    ),
    { requireSync: true },
  )

  readonly provider = toSignal(
    this.form.controls.provider.valueChanges.pipe(
      startWith(this.form.controls.provider.value),
    ),
    { requireSync: true },
  )

  readonly providerFields = computed(() => getProviderFields(this.provider()))

  constructor() {
    // Reset form when data loads
    effect(() => {
      const data = this.service.data()
      if (data && this.form.pristine) {
        this.form.reset(data)
        updateDdnsValidators(this.form, data.enabled, data.provider)
      }
    })

    // Update validators when enabled or provider changes
    effect(() => {
      updateDdnsValidators(this.form, this.enabled(), this.provider())
    })
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
    } else if (await this.service.save(this.form.getRawValue())) {
      this.form.markAsPristine()
    }
  }
}
