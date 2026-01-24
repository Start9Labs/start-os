import { Clipboard } from '@angular/cdk/clipboard'
import { Component, inject, input } from '@angular/core'
import { tuiInjectElement } from '@taiga-ui/cdk'
import { TUI_ICON_END, TuiNotificationService } from '@taiga-ui/core'

@Component({
  selector: '[appCopy]',
  template: '<ng-content />',
  providers: [{ provide: TUI_ICON_END, useValue: '@tui.copy' }],
  styles: `
    :host::after {
      opacity: 0;
      transition: opacity var(--tui-duration) ease-in-out !important;
    }

    :host:hover::after,
    :host:focus-visible::after {
      opacity: 1;
    }
  `,
  host: { '(click)': 'copy()' },
})
export class Copy {
  private readonly el = tuiInjectElement()
  private readonly clipboard = inject(Clipboard)
  private readonly alerts = inject(TuiNotificationService)

  public readonly appCopy = input('')

  protected copy() {
    this.clipboard.copy(this.appCopy() || this.el.textContent.trim())
    this.alerts.open('Copied', { appearance: 'positive' }).subscribe()
  }
}
