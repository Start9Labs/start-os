import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { MaskitoDirective } from '@maskito/angular'
import { MaskitoOptions } from '@maskito/core'
import { TuiError, TuiLabel, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TUI_VALIDATION_ERRORS, TuiFieldErrorPipe } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
import { PREFIX_VALIDATION_ERRORS } from './utils'

@Component({
  selector: 'lan-ipv6-subnet',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Subnet</h2></header>
    <section>
      <div>
        <tui-textfield>
          <label tuiLabel>Prefix Length*</label>
          <input tuiTextfield formControlName="prefix" [maskito]="prefixMask" />
        </tui-textfield>
        <tui-error
          formControlName="prefix"
          [error]="[] | tuiFieldError | async"
        />
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
  hostDirectives: [FormSection],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useValue: PREFIX_VALIDATION_ERRORS,
    },
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    MaskitoDirective,
    TuiHeader,
    TuiTitle,
    TuiLabel,
    TuiTextfield,
    TuiError,
    TuiFieldErrorPipe,
  ],
})
export class LanIpv6Subnet {
  protected readonly prefixMask: MaskitoOptions = {
    mask: ['/', /\d/, /\d/, /\d/],
  }
}
