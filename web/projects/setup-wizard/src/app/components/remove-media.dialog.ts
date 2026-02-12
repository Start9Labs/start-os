import { Component } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'

@Component({
  standalone: true,
  imports: [TuiButton, i18nPipe],
  template: `
    <div class="animation-container">
      <div class="port">
        <div class="port-inner"></div>
      </div>
      <div class="usb-stick">
        <div class="usb-connector"></div>
        <div class="usb-body"></div>
      </div>
    </div>
    <p>
      {{
        'Remove USB stick or other installation media from your server' | i18n
      }}
    </p>
    <footer>
      <button tuiButton (click)="context.completeWith(true)">
        {{ 'Done' | i18n }}
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

    .animation-container {
      position: relative;
      width: 160px;
      height: 69px;
    }

    .port {
      position: absolute;
      left: 20px;
      top: 50%;
      transform: translateY(-50%);
      width: 28px;
      height: 18px;
      background: var(--tui-background-neutral-1);
      border: 2px solid var(--tui-border-normal);
      border-radius: 2px;
    }

    .port-inner {
      position: absolute;
      top: 3px;
      left: 3px;
      right: 3px;
      bottom: 3px;
      background: var(--tui-background-neutral-2);
      border-radius: 1px;
    }

    .usb-stick {
      position: absolute;
      top: 50%;
      transform: translateY(-50%);
      display: flex;
      align-items: center;
      animation: slide-out 2s ease-in-out 0.5s infinite;
      left: 32px;
    }

    .usb-connector {
      width: 20px;
      height: 12px;
      background: var(--tui-text-secondary);
      border-radius: 1px;
    }

    .usb-body {
      width: 40px;
      height: 20px;
      background: var(--tui-status-info);
      border-radius: 2px 4px 4px 2px;
    }

    @keyframes slide-out {
      0% {
        left: 32px;
        opacity: 0;
      }
      5% {
        left: 32px;
        opacity: 1;
      }
      80% {
        left: 130px;
        opacity: 0;
      }
      100% {
        left: 130px;
        opacity: 0;
      }
    }

    p {
      margin: 0 0 2rem;
    }

    footer {
      display: flex;
      justify-content: center;
    }
  `,
})
export class RemoveMediaDialog {
  protected readonly context = injectContext<TuiDialogContext<boolean>>()
}
