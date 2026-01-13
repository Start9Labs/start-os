import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { FormsModule } from '@angular/forms'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import { TuiButton, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiChevron, TuiDataListWrapper, TuiSelect } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { StateService } from '../services/state.service'
import {
  LANGUAGES,
  Language,
  getDefaultKeyboard,
  needsKeyboardSelection,
} from '../utils/languages'

@Component({
  template: `
    <section tuiCardLarge="compact">
      <header tuiHeader>
        <h2 tuiTitle>
          <span class="inline-title">
            <img src="assets/img/icon.png" alt="Start9" />
            Welcome to StartOS
          </span>
          <span tuiSubtitle>Select your language</span>
        </h2>
      </header>

      <tui-textfield
        tuiChevron
        [stringify]="stringify"
        [tuiTextfieldCleaner]="false"
      >
        <label tuiLabel>Language</label>
        @if (mobile) {
          <select tuiSelect [(ngModel)]="selected" [items]="languages"></select>
        } @else {
          <input tuiSelect [(ngModel)]="selected" />
        }
        @if (!mobile) {
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            new
            [items]="languages"
            [itemContent]="itemContent"
          />
        }
      </tui-textfield>

      <ng-template #itemContent let-item>
        <div class="language-item">
          <span>{{ item.nativeName }}</span>
          @if (item.name !== item.nativeName) {
            <small>{{ item.name }}</small>
          }
        </div>
      </ng-template>

      <footer>
        <button tuiButton [disabled]="!selected" (click)="continue()">
          Continue
        </button>
      </footer>
    </section>
  `,
  styles: `
    :host {
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 100%;
    }

    .language-item {
      display: flex;
      flex-direction: column;

      small {
        opacity: 0.7;
      }
    }
  `,
  imports: [
    FormsModule,
    TuiCardLarge,
    TuiButton,
    TuiTextfield,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    TuiHeader,
    TuiTitle,
  ],
})
export default class LanguagePage {
  private readonly router = inject(Router)
  private readonly stateService = inject(StateService)

  protected readonly mobile = inject(TUI_IS_MOBILE)
  readonly languages = LANGUAGES
  selected =
    LANGUAGES.find(l => l.code === this.stateService.language) || LANGUAGES[0]

  readonly stringify = (lang: Language) => lang.nativeName

  async continue() {
    if (this.selected) {
      this.stateService.language = this.selected.code

      if (this.stateService.kiosk) {
        // Check if we need keyboard selection
        if (needsKeyboardSelection(this.selected.code)) {
          await this.router.navigate(['/keyboard'])
        } else {
          // Auto-select the only keyboard option
          this.stateService.keyboard = getDefaultKeyboard(
            this.selected.code,
          ).code
          await this.navigateToNextStep()
        }
      } else {
        await this.navigateToNextStep()
      }
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
