import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { MaskitoDirective } from '@maskito/angular'
import { MaskitoOptions } from '@maskito/core'
import {
  TuiError,
  TuiInput,
  TuiTextfield,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiCardLarge, TuiForm } from '@taiga-ui/layout'
import { FORM } from 'src/app/directives/form'

import { PREFIX_VALIDATION_ERRORS } from '../utils'

@Component({
  selector: 'lan-ipv6-subnet',
  template: `
    <tui-textfield>
      <label tuiLabel>Subnet Prefix Length*</label>
      <input tuiInput formControlName="prefix" [maskito]="prefixMask" />
    </tui-textfield>
    <tui-error formControlName="prefix" />
  `,
  styles: `
    section {
      flex-direction: column !important;
    }

    header {
      padding-top: 1rem;
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
