import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { MaskitoDirective } from '@maskito/angular'
import { TUI_FALSE_HANDLER, TuiAnimated } from '@taiga-ui/cdk'
import {
  TuiError,
  TuiHint,
  TuiInput,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiRadioList } from '@taiga-ui/kit'
import {
  TuiCardLarge,
  TuiElasticContainer,
  TuiForm,
  TuiHeader,
} from '@taiga-ui/layout'
import { FORM } from 'src/app/components/form'
import { PREFIX } from 'src/app/utils/masks'
import WanIpv6 from '../'
import {
  IPV6_ALL_CONTROLS,
  IPV6_CONTROLS,
  IPV6_LABELS,
  IPV6_MODES,
  IPV6_VALIDATION_ERRORS,
} from '../utils'

@Component({
  selector: 'wan-ipv6-ip',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>IP Address</h2></header>
    <tui-radio-list
      size="s"
      formControlName="mode"
      [items]="modes"
      [itemContent]="template"
      [disabledItemHandler]="handler()"
    />
    <ng-template #template let-item>
      {{ $any(labels)[item] }}{{ item === 'slaac' ? ' (Default)' : '' }}
      <i tuiHint="Published ports are using IPv6"></i>
    </ng-template>
    <tui-elastic-container>
      @if (parent.ipMode() !== 'disabled') {
        <section tuiAnimated>
          @for (name of all; track name) {
            @let auto = ['slaac', 'dhcpv6'].includes(parent.ipMode());
            @let optional = name === 'prefix' && auto;

            @if (controls[parent.ipMode()]?.includes(name)) {
              <div>
                <tui-textfield>
                  <label tuiLabel>
                    {{ labels[name] }}{{ optional ? ' (optional)' : '' }}
                  </label>
                  <input
                    tuiInput
                    [formControlName]="name"
                    [maskito]="['prefix', 'mask'].includes(name) ? mask : null"
                    [placeholder]="optional ? 'Auto' : ''"
                  />
                </tui-textfield>
                <tui-error [formControlName]="name" />
              </div>
            }
          }
        </section>
      }
    </tui-elastic-container>
  `,
  styles: `
    tui-radio-list {
      flex-direction: row;
      gap: inherit;
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(IPV6_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiRadioList,
    TuiError,
    TuiInput,
    TuiHint,
    MaskitoDirective,
    TuiAnimated,
    TuiElasticContainer,
  ],
})
export class WanIpv6Ip {
  protected readonly parent = inject(WanIpv6)
  protected readonly modes = IPV6_MODES
  protected readonly labels = IPV6_LABELS
  protected readonly controls = IPV6_CONTROLS
  protected readonly all = IPV6_ALL_CONTROLS
  protected readonly mask = PREFIX
  protected readonly handler = computed(() =>
    this.parent.hasIpv6Ports()
      ? (item: string) => item === 'ddisabled'
      : TUI_FALSE_HANDLER,
  )
}
