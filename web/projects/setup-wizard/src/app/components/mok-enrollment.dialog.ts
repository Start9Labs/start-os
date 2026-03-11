import { Component } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiIcon } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'

@Component({
  standalone: true,
  imports: [TuiButton, TuiIcon, i18nPipe],
  template: `
    <div class="icon-container">
      <tui-icon icon="@tui.shield-check" class="mok-icon" />
    </div>
    <h3>{{ 'Secure Boot Key Enrollment' | i18n }}</h3>
    <p>
      {{
        'A signing key was enrolled for Secure Boot. On the next reboot, a blue screen (MokManager) will appear.'
          | i18n
      }}
    </p>
    <ol>
      <li>Select "Enroll MOK"</li>
      <li>Select "Continue"</li>
      <li>{{ 'Enter your StartOS master password when prompted' | i18n }}</li>
      <li>Select "Reboot"</li>
    </ol>
    <footer>
      <button tuiButton (click)="context.completeWith(true)">
        {{ 'Got it' | i18n }}
      </button>
    </footer>
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      align-items: center;
      text-align: center;
    }

    .icon-container {
      margin-bottom: 1rem;
    }

    .mok-icon {
      width: 3rem;
      height: 3rem;
      color: var(--tui-status-info);
    }

    h3 {
      margin: 0 0 0.5rem;
    }

    p {
      margin: 0 0 1rem;
      color: var(--tui-text-secondary);
    }

    ol {
      text-align: left;
      margin: 0 0 1.5rem;
      padding-left: 1.5rem;

      li {
        margin-bottom: 0.25rem;
      }
    }

    footer {
      display: flex;
      justify-content: center;
    }
  `,
})
export class MokEnrollmentDialog {
  protected readonly context = injectContext<TuiDialogContext<boolean>>()
}
