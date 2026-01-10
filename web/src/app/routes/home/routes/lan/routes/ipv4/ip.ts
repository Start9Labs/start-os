import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiLabel, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TUI_VALIDATION_ERRORS,
  TuiChevron,
  TuiDataListWrapper,
  TuiFieldErrorPipe,
  TuiInputNumber,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { FORM, FormSection } from 'src/app/directives/form'
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
    <header tuiHeader="body-l"><h2 tuiTitle>IP Addresses</h2></header>
    <section>
      <div>
        <label tuiLabel>Address Range*</label>
        <div class="ip-group">
          <tui-textfield tuiChevron [tuiTextfieldCleaner]="false">
            <input tuiSelect formControlName="firstOctet" />
            <tui-data-list-wrapper
              *tuiTextfieldDropdown
              new
              [items]="firstOctets"
            />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiTextfield [value]="secondOctet()" disabled />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input
              tuiInputNumber
              formControlName="thirdOctet"
              [min]="0"
              [max]="255"
            />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiTextfield value="x" disabled />
          </tui-textfield>
        </div>
        <tui-error
          formControlName="thirdOctet"
          [error]="[] | tuiFieldError | async"
        />
      </div>
      <div>
        <label tuiLabel>Router IP*</label>
        <div class="ip-group">
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiTextfield [value]="firstOctet$()" disabled />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiTextfield [value]="secondOctet()" disabled />
          </tui-textfield>
          <tui-textfield [tuiTextfieldCleaner]="false">
            <input tuiTextfield [value]="thirdOctet$()" disabled />
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
        <tui-error
          formControlName="routerOctet"
          [error]="[] | tuiFieldError | async"
        />
      </div>
    </section>
  `,
  styles: `
    .ip-group {
      display: flex;
      gap: 0.25rem;

      tui-textfield {
        width: 4.5rem;
      }
    }

    section > div {
      min-width: 20rem;
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useValue: LAN_IPV4_VALIDATION_ERRORS,
    },
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiLabel,
    TuiTextfield,
    TuiError,
    TuiFieldErrorPipe,
    TuiInputNumber,
    TuiSelect,
    TuiChevron,
    TuiDataListWrapper,
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

  readonly thirdOctet$ = toSignal(
    this.parent.form.controls.ip.controls.thirdOctet.valueChanges.pipe(
      startWith(this.parent.form.controls.ip.controls.thirdOctet.value),
    ),
    { requireSync: true },
  )

  readonly secondOctet = computed(() =>
    getSecondOctet(this.firstOctet$() as FirstOctet),
  )
}
