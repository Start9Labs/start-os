import {
  ChangeDetectionStrategy,
  Component,
  effect,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import {
  TuiDataList,
  TuiInput,
  TuiLabel,
  TuiRadio,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { TuiChevron, TuiSelect, TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm } from '@taiga-ui/layout'
import { startWith } from 'rxjs'
import { Help } from 'src/app/directives/help'
import { WifiSettingsAside } from './aside'
import { WifiService } from '../../service'

@Component({
  template: `
    <wifi-settings-aside *help />
    <form tuiForm="m" tuiCardLarge class="g-form" [formGroup]="form">
      <label tuiLabel>
        <input type="checkbox" tuiSwitch formControlName="enabled" />
        Enable Wi-Fi
      </label>
      <tui-textfield>
        <label tuiLabel>SSID</label>
        <input tuiInput formControlName="ssid" />
      </tui-textfield>
      <label tuiLabel>
        <input type="checkbox" tuiSwitch formControlName="broadcast" />
        Broadcast
      </label>
      <fieldset>
        <legend>Frequency Band</legend>
        @for (value of bands; track $index) {
          <label tuiLabel>
            <input
              type="radio"
              tuiRadio
              formControlName="band"
              [value]="value"
            />
            {{ value }}
          </label>
        }
      </fieldset>
      @if (band() === 'Both') {
        <label tuiLabel>
          <input
            type="checkbox"
            tuiSwitch
            formControlName="broadcastSeparately"
          />
          Broadcast Separately
        </label>
      }
      <fieldset>
        <legend>Frequency Range</legend>
        <!-- @TODO: Implement channel optimization (requires backend channel scan endpoint) -->
        <tui-textfield tuiChevron>
          <label tuiLabel>2.4 GHz Channel</label>
          <input tuiSelect formControlName="channel24" />
          <tui-data-list *tuiDropdown>
            @for (ch of channels24; track ch) {
              <button tuiOption [value]="ch">{{ ch }}</button>
            }
          </tui-data-list>
        </tui-textfield>
        <tui-textfield tuiChevron>
          <label tuiLabel>5 GHz Channel</label>
          <input tuiSelect formControlName="channel5" />
          <tui-data-list *tuiDropdown>
            @for (ch of channels5; track ch) {
              <button tuiOption [value]="ch">{{ ch }}</button>
            }
          </tui-data-list>
        </tui-textfield>
      </fieldset>
    </form>
  `,
  styles: `
    fieldset {
      display: flex !important;
    }

    tui-textfield {
      flex: 1;
      max-width: 16rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [tuiTextfieldOptionsProvider({ cleaner: signal(false) })],
  host: { class: 'g-page' },
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiCardLarge,
    TuiLabel,
    TuiSwitch,
    TuiRadio,
    TuiTextfield,
    TuiInput,
    TuiSelect,
    TuiChevron,
    TuiDataList,
    WifiSettingsAside,
    Help,
  ],
})
export default class WifiSettings {
  private readonly service = inject(WifiService)

  protected readonly form = inject(NonNullableFormBuilder).group({
    enabled: [true],
    ssid: ['StartOS'],
    broadcast: [true],
    band: ['Both'],
    broadcastSeparately: [false],
    channel24: ['Auto'],
    channel5: ['Auto'],
  })

  protected readonly band = toSignal(
    this.form.controls.band.valueChanges.pipe(
      startWith(this.form.controls.band.value),
    ),
    { requireSync: true },
  )

  protected readonly bands = ['2.4 GHz', '5 GHz', 'Both']

  protected readonly channels24 = [
    'Auto',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '10',
    '11',
  ]

  protected readonly channels5 = [
    'Auto',
    '36',
    '40',
    '44',
    '48',
    '52',
    '56',
    '60',
    '64',
    '100',
    '104',
    '108',
    '112',
    '116',
    '120',
    '124',
    '128',
    '132',
    '136',
    '140',
    '144',
    '149',
    '153',
    '157',
    '161',
    '165',
  ]

  constructor() {
    effect(() => {
      const config = this.service.data()
      if (!config) return

      const radios = Object.entries(config.radios)
      const radio2g = radios.find(([, r]) => r.band === '2g')
      const radio5g = radios.find(([, r]) => r.band === '5g')
      const anyEnabled = radios.some(([, r]) => r.enabled)
      const anyBroadcast = radios.some(([, r]) => r.broadcast)

      const channelToOption = (ch: string) => (ch === 'auto' ? 'Auto' : ch)

      this.form.patchValue(
        {
          enabled: anyEnabled,
          ssid: config.ssid,
          broadcast: anyBroadcast,
          band: radio2g && radio5g ? 'Both' : radio5g ? '5 GHz' : '2.4 GHz',
          channel24: radio2g ? channelToOption(radio2g[1].channel) : 'Auto',
          channel5: radio5g ? channelToOption(radio5g[1].channel) : 'Auto',
        },
        { emitEvent: false },
      )
    })
  }
}
