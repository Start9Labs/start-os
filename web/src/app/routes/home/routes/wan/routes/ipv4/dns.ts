import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiRadio,
  TuiSelect,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'

import Ipv4 from '.'
import { LABELS } from './utils'

@Component({
  selector: 'ipv4-dns',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>DNS Server</h2></header>
    <section>
      @for (mode of ['isp', 'tls', 'custom']; track $index) {
        <label tuiLabel>
          <input type="radio" tuiRadio formControlName="mode" [value]="mode" />
          {{ labels[mode] }}{{ $index ? '' : ' (Default)' }}
        </label>
      }
    </section>
    @if (parent.dns === 'tls') {
      <section>
        <tui-textfield tuiChevron [tuiTextfieldCleaner]="false">
          <label tuiLabel>DNS Server*</label>
          <input tuiSelect formControlName="server" />
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            new
            [items]="['Cloudflare (1.1.1.1)', 'something.else']"
          />
        </tui-textfield>
      </section>
    }
    @if (parent.dns === 'custom') {
      <section>
        <tui-textfield>
          <label tuiLabel>Priority 1*</label>
          <input tuiTextfield formControlName="1" />
        </tui-textfield>
        <label tuiLabel>
          <input type="checkbox" tuiSwitch formControlName="tls1" />
          TLS
        </label>
      </section>
      <section>
        <tui-textfield>
          <label tuiLabel>Priority 2</label>
          <input tuiTextfield formControlName="2" />
        </tui-textfield>
        <label tuiLabel>
          <input type="checkbox" tuiSwitch formControlName="tls2" />
          TLS
        </label>
      </section>
    }
    @if (parent.dns !== 'tls') {
      <label tuiLabel>
        <input type="checkbox" tuiSwitch formControlName="proxy" />
        Use DNScrypt Proxy
      </label>
      @if (parent.form.controls.dns.value.proxy) {
        <section>
          <tui-textfield tuiChevron [tuiTextfieldCleaner]="false">
            <label tuiLabel>Proxy Server*</label>
            <input tuiSelect formControlName="server" />
            <tui-data-list-wrapper
              *tuiTextfieldDropdown
              new
              [items]="['dns.watch', 'something.else']"
            />
          </tui-textfield>
        </section>
      }
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiRadio,
    TuiDataListWrapper,
    TuiSelect,
    TuiChevron,
    TuiSwitch,
  ],
})
export class Ipv4Dns {
  protected readonly parent = inject(Ipv4)
  protected readonly labels = LABELS
}
