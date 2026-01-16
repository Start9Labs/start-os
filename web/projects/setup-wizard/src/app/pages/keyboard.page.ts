import { Component, inject, signal } from '@angular/core'
import { Router } from '@angular/router'
import { FormsModule } from '@angular/forms'
import {
  getAllKeyboardsSorted,
  i18nPipe,
  Keyboard,
  LanguageCode,
} from '@start9labs/shared'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import { TuiButton, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TuiButtonLoading,
  TuiChevron,
  TuiDataListWrapper,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { ApiService } from '../services/api.service'
import { StateService } from '../services/state.service'

@Component({
  template: `
    <section tuiCardLarge="compact">
      <header tuiHeader>
        <h2 tuiTitle>{{ 'Select Keyboard Layout' | i18n }}</h2>
      </header>
      <tui-textfield
        tuiChevron
        [stringify]="stringify"
        [tuiTextfieldCleaner]="false"
      >
        <label tuiLabel>{{ 'Keyboard' | i18n }}</label>
        @if (mobile) {
          <select tuiSelect [(ngModel)]="selected" [items]="keyboards"></select>
        } @else {
          <input tuiSelect [(ngModel)]="selected" />
        }
        @if (!mobile) {
          <tui-data-list-wrapper
            new
            *tuiTextfieldDropdown
            [items]="keyboards"
          />
        }
      </tui-textfield>

      <footer>
        <button
          tuiButton
          [disabled]="!selected"
          [loading]="saving()"
          (click)="continue()"
        >
          {{ 'Continue' | i18n }}
        </button>
      </footer>
    </section>
  `,
  imports: [
    FormsModule,
    TuiCardLarge,
    TuiButton,
    TuiButtonLoading,
    TuiTextfield,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    TuiHeader,
    TuiTitle,
    i18nPipe,
  ],
})
export default class KeyboardPage {
  private readonly router = inject(Router)
  private readonly api = inject(ApiService)
  private readonly stateService = inject(StateService)

  protected readonly mobile = inject(TUI_IS_MOBILE)
  // All keyboards, with language-specific keyboards at the top
  readonly keyboards = getAllKeyboardsSorted(
    this.stateService.language as LanguageCode,
  )
  selected =
    this.keyboards.find(k => k.code === this.stateService.keyboard) ||
    this.keyboards[0]!

  readonly saving = signal(false)

  readonly stringify = (kb: Keyboard) => kb.name

  async continue() {
    this.saving.set(true)

    try {
      // Send keyboard to backend
      await this.api.setKeyboard({
        layout: this.selected.code,
        model: null,
        variant: null,
        options: [],
      })

      this.stateService.keyboard = this.selected.code
      await this.navigateToNextStep()
    } finally {
      this.saving.set(false)
    }
  }

  private async navigateToNextStep() {
    if (this.stateService.dataDriveGuid) {
      if (this.stateService.attach) {
        this.stateService.setupType = 'attach'
        await this.router.navigate(['/password'])
      } else {
        await this.router.navigate(['/home'])
      }
    } else {
      await this.router.navigate(['/drives'])
    }
  }
}
