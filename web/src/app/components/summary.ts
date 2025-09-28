import { CdkCopyToClipboard } from '@angular/cdk/clipboard'
import {
  ChangeDetectionStrategy,
  Component,
  Directive,
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
import { TuiForm } from '@taiga-ui/layout'

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

      [tuiBadge] {
        margin: -0.125rem;
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
export class SummaryItem {
  public readonly appSummary = input('', {
    transform: (value?: string) => value || '',
  })
}

@Directive({
  host: {
    class: 'g-form',
    '[style.background]': '"var(--tui-status-info-pale)"',
  },
  hostDirectives: [TuiForm],
})
export class Summary {}
