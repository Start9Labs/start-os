import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { MaskitoDirective } from '@maskito/angular'
import { MaskitoOptions } from '@maskito/core'
import {
  TuiError,
  TuiInput,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/directives/form'
import { PREFIX_VALIDATION_ERRORS } from './utils'

@Component({
  selector: 'lan-ipv6-subnet',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Subnet</h2></header>
    <section>
      <div>
        <tui-textfield>
          <label tuiLabel>Prefix Length*</label>
          <input tuiInput formControlName="prefix" [maskito]="prefixMask" />
        </tui-textfield>
        <tui-error formControlName="prefix" />
      </div>
    </section>
  `,
  styles: `
    section {
      flex-direction: column !important;
    }

    tui-textfield {
      max-width: 10rem;
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(PREFIX_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    MaskitoDirective,
    TuiHeader,
    TuiTitle,
    TuiInput,
    TuiTextfield,
    TuiError,
  ],
})
export class LanIpv6Subnet {
  protected readonly prefixMask: MaskitoOptions = {
    mask: ['/', /\d/, /\d/, /\d/],
  }
}
