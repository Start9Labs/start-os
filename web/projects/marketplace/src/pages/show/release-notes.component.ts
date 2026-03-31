import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { LocalizePipe, MarkdownPipe } from '@start9labs/shared'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import { MarketplacePkgBase } from '../../types'

@Component({
  selector: 'marketplace-release-notes',
  template: `
    <div class="background-border box-shadow-lg shadow-color-light">
      <div class="box-container">
        <h2 class="additional-detail-title">New in {{ pkg().version }}</h2>
        <p [innerHTML]="pkg().releaseNotes | localize | markdown | dompurify"></p>
      </div>
    </div>
  `,
  styles: `
    .box-container {
      background-color: rgb(39 39 42);
      border-radius: 0.75rem;
      padding: 1.25rem 1.75rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [NgDompurifyPipe, MarkdownPipe, LocalizePipe],
})
export class MarketplaceReleaseNotesComponent {
  readonly pkg = input.required<MarketplacePkgBase>()
}
