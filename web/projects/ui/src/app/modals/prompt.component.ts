import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiAutoFocusModule } from '@taiga-ui/cdk'
import { TuiDialogContext, TuiTextfieldControllerModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiInputModule } from '@taiga-ui/kit'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'

@Component({
  standalone: true,
  template: `
    <p>{{ options.message }}</p>
    <p *ngIf="options.warning" class="warning">{{ options.warning }}</p>
    <form (ngSubmit)="submit(value.trim())">
      <tui-input
        tuiAutoFocus
        [tuiTextfieldLabelOutside]="!options.label"
        [tuiTextfieldCustomContent]="options.useMask ? toggle : ''"
        [ngModelOptions]="{ standalone: true }"
        [(ngModel)]="value"
      >
        {{ options.label }}
        <span *ngIf="options.required !== false && options.label">*</span>
        <input
          tuiTextfield
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
          Cancel
        </button>
        <button tuiButton [disabled]="!value && options.required !== false">
          {{ options.buttonText || 'Submit' }}
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
        [iconLeft]="masked ? 'tuiIconEye' : 'tuiIconEyeOff'"
        (click)="masked = !masked"
      ></button>
    </ng-template>
  `,
  styles: [
    `
      .warning {
        color: var(--tui-warning-fill);
      }

      .button {
        pointer-events: auto;
        margin-left: 0.25rem;
      }

      .masked {
        -webkit-text-security: disc;
      }
    `,
  ],
  imports: [
    CommonModule,
    FormsModule,
    TuiInputModule,
    TuiButtonModule,
    TuiTextfieldControllerModule,
    TuiAutoFocusModule,
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
  message: string
  label?: string
  warning?: string
  buttonText?: string
  placeholder?: string
  required?: boolean
  useMask?: boolean
  initialValue?: string | null
}
