import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiDropdown,
  TuiError,
  TuiGroup,
  TuiInput,
  TuiTextfield,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiDataListWrapper, TuiInputNumber, TuiSelect } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { FORM } from 'src/app/components/form'
import LanIpv4 from '../'
import {
  FIRST_OCTETS,
  FirstOctet,
  getSecondOctet,
  LAN_IPV4_VALIDATION_ERRORS,
} from '../utils'

@Component({
  selector: 'lan-ipv4-ip',
  template: `
    <fieldset [tuiTextfieldCleaner]="false">
      <legend>Network Block</legend>
      <div tuiGroup>
        <tui-textfield tuiDropdownLimitWidth="min">
          <input tuiSelect formControlName="firstOctet" />
          <tui-data-list-wrapper *tuiDropdown [items]="firstOctets" />
        </tui-textfield>
        <tui-textfield>
          <input tuiInput [value]="secondOctet()" disabled />
        </tui-textfield>
        <tui-textfield>
          <input tuiInput value="x" disabled />
        </tui-textfield>
        <tui-textfield>
          <input tuiInput value="x" disabled />
        </tui-textfield>
        /16
      </div>
    </fieldset>
    <fieldset [tuiTextfieldCleaner]="false">
      <legend>Router IP</legend>
      <div tuiGroup>
        <tui-textfield>
          <input tuiInput [value]="firstOctet$()" disabled />
        </tui-textfield>
        <tui-textfield>
          <input tuiInput [value]="secondOctet()" disabled />
        </tui-textfield>
        <tui-textfield>
          <input tuiInput value="0" disabled />
        </tui-textfield>
        <tui-textfield>
          <input
            tuiInputNumber
            formControlName="routerOctet"
            [min]="1"
            [max]="254"
          />
        </tui-textfield>
      </div>
    </fieldset>
    <tui-error formControlName="routerOctet" />
  `,
  styles: `
    tui-textfield {
      max-inline-size: 3.25rem;
    }

    div {
      align-items: center;
      text-indent: 0.5rem;
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(LAN_IPV4_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiTextfield,
    TuiError,
    TuiInputNumber,
    TuiSelect,
    TuiDataListWrapper,
    TuiInput,
    TuiGroup,
    TuiDropdown,
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
