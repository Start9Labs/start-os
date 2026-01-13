import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { WA_LOCAL_STORAGE } from '@ng-web-apis/common'
import { TuiValueChanges } from '@taiga-ui/cdk'
import {
  TUI_DARK_MODE,
  TUI_DARK_MODE_KEY,
  TuiAppearance,
  TuiButton,
  tuiButtonOptionsProvider,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiAccordion,
  TuiChevron,
  TuiDataListWrapper,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'

import { GeneralAside } from './aside'
import { GeneralSummary } from './summary'

@Component({
  template: `
    <general-aside *help />
    <section tuiCardLarge appearance="positive">
      <header tuiHeader="body-l" [style.margin]="0">
        <h2 tuiTitle>v1.0.1 released!</h2>
        <aside tuiAccessories>
          <button tuiButton appearance="outline-grayscale">
            Release notes
          </button>
          <button tuiButton appearance="positive">Update now</button>
        </aside>
      </header>
    </section>
    <article generalSummary [formLoading]="false"></article>
    <form tuiForm="m" tuiCardLarge class="g-form" [formGroup]="form">
      <header tuiHeader>
        <h2 tuiTitle>Preferences</h2>
      </header>
      <fieldset>
        <tui-textfield tuiChevron>
          <label tuiLabel>Theme</label>
          <input
            tuiSelect
            formControlName="theme"
            (tuiValueChanges)="onTheme($event)"
          />
          <tui-data-list-wrapper
            *tuiDropdown
            [items]="['System', 'Dark', 'Light']"
          />
        </tui-textfield>
        <tui-textfield tuiChevron>
          <label tuiLabel>Language</label>
          <input tuiSelect formControlName="language" />
          <tui-data-list-wrapper *tuiDropdown [items]="['English']" />
        </tui-textfield>
      </fieldset>
      <fieldset>
        <tui-textfield tuiChevron>
          <label tuiLabel>Timezone</label>
          <input tuiSelect formControlName="timezone" />
          <tui-data-list-wrapper *tuiDropdown [items]="['UTC']" />
        </tui-textfield>
        <tui-textfield tuiChevron>
          <label tuiLabel>Week start day</label>
          <input tuiSelect formControlName="week" />
          <tui-data-list-wrapper
            *tuiDropdown
            [items]="['Monday', 'Sunday', 'Saturday']"
          />
        </tui-textfield>
      </fieldset>
    </form>
    <form tuiForm="m" tuiCardLarge class="g-form">
      <header tuiHeader>
        <h2 tuiTitle>Power</h2>
      </header>
      <footer>
        <button tuiButton>Restart</button>
        <button tuiButton>Shut down</button>
      </footer>
    </form>
    <tui-accordion tuiAppearance="neutral" [style.border-radius.rem]="1">
      <button tuiAccordion appearance="">
        <header tuiHeader="h6" [style.left]="0">
          <h2 tuiTitle>Advanced</h2>
        </header>
      </button>
      <tui-expand>
        <footer>
          <button tuiButton>Launch LuCI Interface</button>
          <button tuiButton>Download Support Diagnostics</button>
          <button tuiButton>Factory Reset</button>
        </footer>
      </tui-expand>
    </tui-accordion>
    <footer class="g-footer">
      <button tuiButton appearance="flat">Cancel</button>
      <button tuiButton appearance="primary">Save</button>
    </footer>
  `,
  styles: `
    :host {
      padding-top: 1rem;
    }

    footer {
      display: flex;
      flex-wrap: wrap;
      gap: 1rem;
    }

    .g-footer {
      padding: 1rem 0 2rem;
    }

    [tuiAccordion] {
      font: var(--tui-font-heading-6);
    }

    tui-expand {
      padding-top: 1px;
      box-shadow: none;
    }
  `,
  host: { class: 'g-page' },
  providers: [
    tuiTextfieldOptionsProvider({ cleaner: signal(false) }),
    tuiButtonOptionsProvider({ appearance: 'outline' }),
  ],
  imports: [
    ReactiveFormsModule,
    TuiCard,
    GeneralSummary,
    GeneralAside,
    Form,
    Help,
    TuiHeader,
    TuiTitle,
    TuiButton,
    TuiAppearance,
    TuiForm,
    TuiTextfield,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    TuiValueChanges,
    TuiAccordion,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class General {
  private readonly mode = inject(TUI_DARK_MODE)
  private readonly system = !inject(WA_LOCAL_STORAGE)?.getItem(
    inject(TUI_DARK_MODE_KEY),
  )

  protected readonly form = inject(NonNullableFormBuilder).group({
    theme: this.system ? 'System' : this.mode() ? 'Dark' : 'Light',
    timezone: 'UTC',
    week: 'Monday',
    language: 'English',
  })

  protected onTheme(theme: string): void {
    if (theme === 'System') {
      this.mode.reset()
    } else {
      this.mode.set(theme === 'Dark')
    }
  }
}
