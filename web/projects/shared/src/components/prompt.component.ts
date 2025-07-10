import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiAutoFocus } from '@taiga-ui/cdk'
import { TuiButton, TuiDialogContext } from '@taiga-ui/core'
import { TuiInputModule, TuiTextfieldControllerModule } from '@taiga-ui/legacy'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import { i18nPipe } from '../i18n/i18n.pipe'
import { i18nKey } from '../i18n/i18n.providers'

@Component({
  template: `
    <p>{{ options.message }}</p>
    @if (options.warning) {
      <p class="warning">{{ options.warning }}</p>
    }
    <form (ngSubmit)="submit(value.trim())">
      <tui-input
        tuiAutoFocus
        [tuiTextfieldLabelOutside]="!options.label"
        [tuiTextfieldCustomContent]="options.useMask ? toggle : ''"
        [ngModelOptions]="{ standalone: true }"
        [(ngModel)]="value"
      >
        {{ options.label }}
        @if (options.required !== false && options.label) {
          <span>*</span>
        }
        <input
          tuiTextfieldLegacy
          [class.masked]="options.useMask && masked && value"
          [placeholder]="options.placeholder || ''"
        />
      </tui-input>
      <footer class="g-buttons">
        <button
          tuiButton
          type="button"
          appearance="secondary"
          (click)="cancel()"
        >
          {{ 'Cancel' | i18n }}
        </button>
        <button tuiButton [disabled]="!value && options.required !== false">
          {{ options.buttonText || ('Submit' | i18n) }}
        </button>
      </footer>
    </form>

    <ng-template #toggle>
      <button
        tuiIconButton
        type="button"
        appearance="icon"
        title="Toggle masking"
        size="xs"
        class="button"
        [iconStart]="masked ? '@tui.eye' : '@tui.eye-off'"
        (click)="masked = !masked"
      ></button>
    </ng-template>
  `,
  styles: `
    .warning {
      color: var(--tui-status-warning);
    }

    .button {
      pointer-events: auto;
      margin-left: 0.25rem;
    }

    .masked {
      -webkit-text-security: disc;
    }
  `,
  imports: [
    FormsModule,
    TuiInputModule,
    TuiButton,
    TuiTextfieldControllerModule,
    TuiAutoFocus,
    i18nPipe,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PromptModal {
  masked = this.options.useMask
  value = this.options.initialValue || ''

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<string, PromptOptions>,
  ) {}

  get options(): PromptOptions {
    return this.context.data
  }

  cancel() {
    this.context.$implicit.complete()
  }

  submit(value: string) {
    if (value || !this.options.required) {
      this.context.$implicit.next(value)
    }
  }
}

export const PROMPT = new PolymorpheusComponent(PromptModal)

export interface PromptOptions {
  message: i18nKey
  label?: i18nKey
  warning?: i18nKey
  buttonText?: i18nKey
  placeholder?: i18nKey
  required?: boolean
  useMask?: boolean
  initialValue?: string | null
}
