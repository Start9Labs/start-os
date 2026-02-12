import { CdkCopyToClipboard, Clipboard } from '@angular/cdk/clipboard'
import { Component, inject, input, linkedSignal, signal } from '@angular/core'
import { tuiInjectElement } from '@taiga-ui/cdk'
import { TUI_ICON_END, TuiButton, TuiNotificationService } from '@taiga-ui/core'

@Component({
  selector: '[appMasked]',
  template: `
    {{ masked() ? '••••••••••••••••' : appMasked() }}
    <button
      tuiIconButton
      size="xs"
      appearance="icon"
      [iconStart]="masked() ? '@tui.eye' : '@tui.eye-off'"
      (click)="masked.set(!masked())"
    >
      Toggle visibility
    </button>
    <button
      tuiIconButton
      size="xs"
      appearance="icon"
      iconStart="@tui.copy"
      [cdkCopyToClipboard]="appMasked()"
      (cdkCopyToClipboardCopied)="
        alerts.open('Copied!', { appearance: 'positive' }).subscribe()
      "
    >
      Copy
    </button>
  `,
  imports: [TuiButton, CdkCopyToClipboard],
})
export class Masked {
  public readonly appMasked = input('')

  protected readonly alerts = inject(TuiNotificationService)
  protected readonly masked = linkedSignal({
    source: this.appMasked,
    computation: () => true,
  })
}
