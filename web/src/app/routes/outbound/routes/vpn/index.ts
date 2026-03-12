import {
  ChangeDetectionStrategy,
  Component,
  computed,
  effect,
  inject,
  signal,
} from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { ActivatedRoute, Router, RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import {
  TuiButton,
  TuiError,
  TuiInput,
  TuiLink,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TUI_CONFIRM,
  TuiChevron,
  TuiDataListWrapper,
  TuiSelect,
  TuiSkeleton,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/components/form'
import { OutboundService } from 'src/app/routes/outbound/service'
import {
  getOutboundVpnForm,
  OUTBOUND_VALIDATION_ERRORS,
} from 'src/app/routes/outbound/utils'
import { CustomValidators } from 'src/app/utils/validators'
import { VPNSummary } from './summary'

@Component({
  template: `
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2>
          <a
            tuiLink
            routerLink=".."
            appearance=""
            iconStart="@tui.chevron-left"
            [tuiSkeleton]="!data()"
            [style.font]="'inherit'"
            [style.text-decoration]="'none'"
          >
            {{ data()?.label || 'Outbound VPN' }}
          </a>
        </h2>
      </hgroup>
    </header>
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article vpnSummary [formLoading]="!data()"></article>
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
            <label tuiLabel>Label</label>
            <input tuiInput formControlName="label" />
          </tui-textfield>
          <tui-error formControlName="label" />
        </div>
      </section>
      <section>
        <div>
          <tui-textfield tuiChevron>
            <label tuiLabel>Connects to</label>
            <input tuiSelect formControlName="target" />
            <tui-data-list-wrapper *tuiDropdown [items]="targetOptions()" />
          </tui-textfield>
        </div>
      </section>
      @if (data()) {
        <footer appFooter>
          <button
            tuiButton
            type="button"
            appearance="secondary-destructive"
            class="g-delete"
            (click)="onDelete()"
          >
            Delete
          </button>
        </footer>
      }
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  providers: [
    tuiTextfieldOptionsProvider({ cleaner: signal(false) }),
    tuiValidationErrorsProvider(OUTBOUND_VALIDATION_ERRORS),
  ],
  imports: [
    RouterLink,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiLink,
    TuiTextfield,
    TuiError,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    TuiButton,
    TuiInput,
    Footer,
    Form,
    VPNSummary,
    TuiSkeleton,
  ],
})
export default class OutboundVPN {
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)
  private readonly dialogs = inject(TuiResponsiveDialogService)

  readonly service = inject(OutboundService)
  readonly vpnId = this.route.snapshot.queryParams['id']
  private readonly otherLabels = computed(() => {
    const allVpns = this.service.data() ?? []
    return allVpns.filter(v => v.id !== this.vpnId).map(v => v.label)
  })
  readonly form = getOutboundVpnForm(inject(NonNullableFormBuilder), [])
  readonly data = computed(
    () => this.service.data()?.find(v => v.id === this.vpnId) ?? null,
  )

  readonly targetOptions = computed(() => {
    const currentLabel = this.data()?.label
    const allVpns = this.service.data() ?? []
    const otherVpns = allVpns
      .filter(v => v.label !== currentLabel)
      .map(v => v.label)
    return ['Internet', ...otherVpns]
  })

  constructor() {
    effect(() => {
      const data = this.data()
      const otherLabels = this.otherLabels()
      if (data && this.form.pristine) {
        this.form.controls.label.setValidators([
          Validators.required,
          CustomValidators.duplicateName(otherLabels),
          CustomValidators.interfaceNameLength('wg_', 15),
        ])
        this.form.reset({ ...data })
      }
    })
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    const success = await this.service.update(
      this.vpnId,
      this.form.getRawValue(),
    )
    if (success) {
      this.form.markAsPristine()
      this.router.navigate(['..'], { relativeTo: this.route })
    }
  }

  onCancel() {
    const data = this.data()
    if (data) {
      this.form.reset({ ...data })
    }
  }

  onDelete() {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        if (await this.service.remove(this.vpnId)) {
          this.router.navigate(['..'], { relativeTo: this.route })
        }
      })
  }
}
