import { CdkCopyToClipboard } from '@angular/cdk/clipboard'
import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary, SummaryItem } from 'src/app/components/summary'

@Component({
  selector: '[securitySummary]',
  template: `
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <label appSummary>
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
      </label>
      <label appSummary>
        SSH
        <span tuiSubtitle>
          <span tuiBadge tuiStatus appearance="positive">Active</span>
        </span>
      </label>
    </section>
  `,
  hostDirectives: [Summary],
  imports: [
    SummaryItem,
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
