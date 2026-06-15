import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { ErrorService } from '@start9labs/shared'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiInput,
  TuiNumberFormat,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiInputNumber,
  TuiNotificationMiddleService,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'

const TYPES = ['A', 'AAAA', 'CNAME', 'TXT'] as const

@Component({
  template: `
    <form tuiForm="m" [formGroup]="form">
      <tui-textfield>
        <label tuiLabel>Name</label>
        <input tuiInput formControlName="name" placeholder="host.example.com" />
      </tui-textfield>
      <tui-error formControlName="name" />
      <tui-textfield tuiChevron [tuiTextfieldCleaner]="false">
        <label tuiLabel>Type</label>
        @if (mobile) {
          <select tuiSelect formControlName="type" [items]="types"></select>
        } @else {
          <input tuiSelect formControlName="type" />
        }
        @if (!mobile) {
          <tui-data-list-wrapper *tuiDropdown [items]="types" />
        }
      </tui-textfield>
      <tui-error formControlName="type" />
      <tui-textfield>
        <label tuiLabel>Value</label>
        <input tuiInput formControlName="value" />
      </tui-textfield>
      <tui-error formControlName="value" />
      <tui-textfield>
        <label tuiLabel>TTL (seconds)</label>
        <input
          tuiInputNumber
          formControlName="ttl"
          [min]="0"
          [tuiNumberFormat]="{ thousandSeparator: '' }"
        />
      </tui-textfield>
      <footer>
        <button tuiButton (click)="onSave()">Save</button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiButton,
    TuiChevron,
    TuiDataListWrapper,
    TuiError,
    TuiInput,
    TuiInputNumber,
    TuiNumberFormat,
    TuiSelect,
    TuiForm,
  ],
})
export class DnsAdd {
  private readonly api = inject(ApiService)
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)

  protected readonly mobile = inject(WA_IS_MOBILE)
  protected readonly context = injectContext<TuiDialogContext<void>>()
  protected readonly types = TYPES

  protected readonly form = inject(NonNullableFormBuilder).group({
    name: ['', Validators.required],
    type: ['A' as (typeof TYPES)[number], Validators.required],
    value: ['', Validators.required],
    ttl: [300 as number | null],
  })

  protected async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    const loader = this.loading.open('').subscribe()
    const { name, type, value, ttl } = this.form.getRawValue()

    try {
      await this.api.addDnsRecord({ name, type, value, ttl })
      this.context.$implicit.complete()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

export const DNS_ADD = new PolymorpheusComponent(DnsAdd)
