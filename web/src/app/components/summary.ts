import { CdkCopyToClipboard } from '@angular/cdk/clipboard'
import {
  ChangeDetectionStrategy,
  Component,
  input,
  ViewEncapsulation,
} from '@angular/core'
import {
  TUI_APPEARANCE_OPTIONS,
  TuiAppearance,
  TuiButton,
  tuiButtonOptionsProvider,
  TuiTitle,
} from '@taiga-ui/core'
import { tuiBadgeOptionsProvider } from '@taiga-ui/kit'

@Component({
  selector: '[appSummary]',
  template: `
    <ng-content />
    @if (appSummary()) {
      <span tuiSubtitle>
        {{ appSummary() }}
        <button
          tuiIconButton
          iconStart="@tui.copy"
          [cdkCopyToClipboard]="appSummary()"
        >
          Copy
        </button>
      </span>
    } @else {
      <ng-content select="[tuiSubtitle]">
        <span tuiSubtitle>-</span>
      </ng-content>
    }
  `,
  styles: `
    [appSummary] {
      padding: 0.5rem 0.75rem;
      border-radius: var(--tui-radius-xs);
      font-weight: bold;

      [tuiIconButton] {
        margin: -0.5rem 0;
      }

      [tuiSubtitle] {
        color: var(--tui-text-secondary);
        display: flex;
        align-items: center;
      }

      > *::first-letter {
        text-transform: capitalize;
      }
    }
  `,
  host: { appSummary: '' },
  hostDirectives: [TuiTitle, TuiAppearance],
  providers: [
    tuiBadgeOptionsProvider({ size: 's' }),
    tuiButtonOptionsProvider({ size: 'xs', appearance: 'icon' }),
    { provide: TUI_APPEARANCE_OPTIONS, useValue: { appearance: 'neutral' } },
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  encapsulation: ViewEncapsulation.None,
  imports: [TuiButton, CdkCopyToClipboard],
})
export class Summary {
  public readonly appSummary = input('', {
    transform: (value?: string) => value || '',
  })
}
