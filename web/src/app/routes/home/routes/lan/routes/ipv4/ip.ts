import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiError,
  TuiInput,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiInputNumber,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { FORM } from 'src/app/directives/form'
import {
  FIRST_OCTETS,
  FirstOctet,
  getSecondOctet,
  LAN_IPV4_VALIDATION_ERRORS,
} from './utils'
import LanIpv4 from '.'

@Component({
  selector: 'lan-ipv4-ip',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Network</h2></header>
    <section>
      <div>
        <label tuiLabel>Network Block*</label>
        <div class="ip-group">
          <tui-textfield tuiChevron [tuiTextfieldCleaner]="false">
            <input tuiSelect formControlName="firstOctet" />
            <tui-data-list-wrapper *tuiDropdown [items]="firstOctets" />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiInput [value]="secondOctet()" disabled />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiInput value="x" disabled />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiInput value="x" disabled />
          </tui-textfield>
          <span class="cidr">/16</span>
        </div>
      </div>
      <div>
        <label tuiLabel>Router IP*</label>
        <div class="ip-group">
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiInput [value]="firstOctet$()" disabled />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiInput [value]="secondOctet()" disabled />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiInput value="0" disabled />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input
              tuiInputNumber
              formControlName="routerOctet"
              [min]="1"
              [max]="254"
            />
          </tui-textfield>
        </div>
        <tui-error formControlName="routerOctet" />
      </div>
    </section>
  `,
  styles: `
    .ip-group {
      display: flex;
      gap: 0.25rem;
      align-items: center;

      tui-textfield {
        width: 5.5rem;
      }

      .cidr {
        font-weight: 500;
        color: var(--tui-text-secondary);
        margin-left: 0.25rem;
      }
    }

    section > div {
      min-width: 25rem;
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(LAN_IPV4_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiError,
    TuiInputNumber,
    TuiSelect,
    TuiChevron,
    TuiDataListWrapper,
    TuiInput,
  ],
})
export class LanIpv4Ip {
  protected readonly parent = inject(LanIpv4)
  protected readonly firstOctets = FIRST_OCTETS

  readonly firstOctet$ = toSignal(
    this.parent.form.controls.ip.controls.firstOctet.valueChanges.pipe(
      startWith(this.parent.form.controls.ip.controls.firstOctet.value),
    ),
    { requireSync: true },
  )

  readonly secondOctet = computed(() =>
    getSecondOctet(this.firstOctet$() as FirstOctet),
  )
}
