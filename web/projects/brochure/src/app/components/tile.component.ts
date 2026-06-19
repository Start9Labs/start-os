import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'
import { LocalizePipe } from '@start9labs/shared'
import { TuiCell, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCardLarge, tuiCardOptionsProvider } from '@taiga-ui/layout'

@Component({
  selector: 'button[marketplaceTile]',
  template: `
    <span tuiCell>
      <span tuiAvatar [round]="false">
        <img
          alt=""
          [src]="pkg().icon || 'assets/img/service-icons/fallback.png'"
        />
      </span>
      <span tuiTitle>
        <b>{{ pkg().title }}</b>
        <span tuiSubtitle>{{ pkg().version }}</span>
      </span>
    </span>
    <span tuiDescription>{{ pkg().description.short | localize }}</span>
  `,
  styles: `
    :host {
      text-align: start;
      box-shadow: none !important;
    }

    [tuiDescription] {
      margin: 0 0 -0.25rem !important;
      color: var(--tui-text-secondary);
      display: -webkit-box;
      -webkit-box-orient: vertical;
      -webkit-line-clamp: 2;
      overflow: hidden;
    }
  `,
  hostDirectives: [TuiCardLarge],
  providers: [
    tuiCardOptionsProvider({ space: 'compact', appearance: 'floating' }),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [LocalizePipe, TuiAvatar, TuiTitle, TuiCell],
})
export class MarketplaceTileComponent {
  readonly pkg = input.required<MarketplacePkg>({ alias: 'marketplaceTile' })
}
