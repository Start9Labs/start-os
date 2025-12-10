import { CdkCopyToClipboard } from '@angular/cdk/clipboard'
import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary } from 'src/app/components/summary'

@Component({
  selector: '[securitySummary]',
  template: `
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <div appSummary>
        Password
        <span tuiSubtitle>
          {{ '*'.repeat(this.password.length) }}
          <button tuiIconButton iconStart="@tui.eye">Show/Hide</button>
          <button
            tuiIconButton
            iconStart="@tui.copy"
            [cdkCopyToClipboard]="password"
          >
            Copy
          </button>
        </span>
      </div>
      <div appSummary>
        SSH
        <span tuiSubtitle>
          <span tuiBadge tuiStatus appearance="positive">Active</span>
        </span>
      </div>
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [
    Summary,
    TuiHeader,
    TuiTitle,
    TuiBadge,
    TuiStatus,
    TuiButton,
    CdkCopyToClipboard,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SecuritySummary {
  protected readonly password = 'hapica12345678'
}
