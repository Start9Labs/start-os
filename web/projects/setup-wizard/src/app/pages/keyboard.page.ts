import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { FormsModule } from '@angular/forms'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import { TuiButton, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiChevron, TuiDataListWrapper, TuiSelect } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { StateService } from '../services/state.service'
import { Keyboard, getKeyboardsForLanguage } from '../utils/languages'

@Component({
  template: `
    <section tuiCardLarge="compact">
      <header tuiHeader>
        <h2 tuiTitle>Select Keyboard Layout</h2>
      </header>
      <tui-textfield
        tuiChevron
        [stringify]="stringify"
        [tuiTextfieldCleaner]="false"
      >
        <label tuiLabel>Keyboard</label>
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

    footer {
      display: flex;
      justify-content: flex-end;
      margin-top: 1.5rem;
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
export default class KeyboardPage {
  private readonly router = inject(Router)
  private readonly stateService = inject(StateService)

  protected readonly mobile = inject(TUI_IS_MOBILE)
  readonly keyboards = getKeyboardsForLanguage(this.stateService.language)
  selected =
    this.keyboards.find(k => k.code === this.stateService.keyboard) ||
    this.keyboards[0]

  readonly stringify = (kb: Keyboard) => kb.name

  async back() {
    await this.router.navigate(['/language'])
  }

  async continue() {
    if (this.selected) {
      this.stateService.keyboard = this.selected.code
      await this.navigateToNextStep()
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
