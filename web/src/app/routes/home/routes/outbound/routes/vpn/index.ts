import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  effect,
  inject,
  signal,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { ActivatedRoute, Router, RouterLink } from '@angular/router'
import {
  TuiButton,
  TuiError,
  TuiLink,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiDialogService } from '@taiga-ui/experimental'
import {
  TUI_CONFIRM,
  TUI_VALIDATION_ERRORS,
  TuiChevron,
  TuiDataListWrapper,
  TuiFieldErrorPipe,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import { OutboundService } from 'src/app/routes/home/routes/outbound/service'
import {
  getOutboundVpnForm,
  OUTBOUND_VALIDATION_ERRORS,
} from 'src/app/routes/home/routes/outbound/utils'
import { VPNAside } from './aside'
import { VPNSummary } from './summary'

@Component({
  template: `
    <vpn-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2>
          <a
            tuiLink
            routerLink=".."
            appearance=""
            iconStart="@tui.chevron-left"
            [style.font]="'inherit'"
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
            <label tuiLabel>Label *</label>
            <input tuiTextfield formControlName="label" />
          </tui-textfield>
          <tui-error
            formControlName="label"
            [error]="[] | tuiFieldError | async"
          />
        </div>
      </section>
      <section>
        <div>
          <tui-textfield tuiChevron>
            <label tuiLabel>Connects to</label>
            <input tuiSelect formControlName="target" />
            <tui-data-list-wrapper
              *tuiTextfieldDropdown
              new
              [items]="targetOptions()"
            />
          </tui-textfield>
        </div>
      </section>
      @if (data()) {
        <footer appFooter [disabled]="form.pristine">
          <button
            tuiButton
            type="button"
            appearance="destructive"
            class="g-delete"
            (click)="onDelete()"
          >
            Delete
          </button>
        </footer>
      }
    </form>
  `,
  styles: `
    :host {
      padding-top: 0;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  providers: [
    tuiTextfieldOptionsProvider({ cleaner: signal(false) }),
    { provide: TUI_VALIDATION_ERRORS, useValue: OUTBOUND_VALIDATION_ERRORS },
  ],
  imports: [
    AsyncPipe,
    RouterLink,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiLink,
    TuiTextfield,
    TuiError,
    TuiFieldErrorPipe,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    TuiButton,
    Footer,
    Form,
    Help,
    VPNAside,
    VPNSummary,
  ],
})
export default class OutboundVPN {
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)
  private readonly dialogs = inject(TuiDialogService)

  readonly service = inject(OutboundService)
  readonly vpnId = this.route.snapshot.params['label']

  readonly form = getOutboundVpnForm(inject(NonNullableFormBuilder))

  readonly data = computed(() => {
    const allVpns = this.service.data()
    return allVpns?.find(v => v.id === this.vpnId) ?? null
  })

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
      if (data && this.form.pristine) {
        this.form.reset({
          label: data.label,
          target: data.target,
        })
      }
    })
  }

  async onSave() {
    if (this.form.invalid) return

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
      this.form.reset({
        label: data.label,
        target: data.target,
      })
    }
  }

  onDelete() {
    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Are you sure?',
        size: 's',
      })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const success = await this.service.remove(this.vpnId)
        if (success) {
          this.router.navigate(['..'], { relativeTo: this.route })
        }
      })
  }
}
