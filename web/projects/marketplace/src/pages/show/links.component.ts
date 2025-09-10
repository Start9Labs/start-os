import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { CopyService, i18nPipe } from '@start9labs/shared'
import { MarketplacePkgBase } from '../../types'
import { MarketplaceLinkComponent } from './link.component'

@Component({
  selector: 'marketplace-links',
  template: `
    <div class="background-border shadow-color-light box-shadow-lg">
      <div class="box-container">
        <h2 class="additional-detail-title">Source Code</h2>
        <div class="detail-container">
          <marketplace-link
            [url]="pkg().upstreamRepo"
            label="Upstream service"
            icon="@tui.external-link"
            class="item-pointer"
          />
          <marketplace-link
            [url]="pkg().wrapperRepo"
            label="StartOS package"
            icon="@tui.external-link"
            class="item-pointer"
          />
        </div>
      </div>
    </div>

    <div class="background-border shadow-color-light box-shadow-lg">
      <div class="box-container">
        <h2 class="additional-detail-title">{{ 'Links' | i18n }}</h2>
        <div class="detail-container">
          <marketplace-link
            [url]="pkg().marketingSite"
            label="Marketing"
            icon="@tui.external-link"
            class="item-pointer"
          />
          @if (pkg().docsUrl; as docsUrl) {
            <marketplace-link
              [url]="docsUrl"
              label="Documentation"
              icon="@tui.external-link"
              class="item-pointer"
            />
          }
          <marketplace-link
            [url]="pkg().supportSite"
            label="Support"
            icon="@tui.external-link"
            class="item-pointer"
          />
          @if (pkg().donationUrl; as donationUrl) {
            <marketplace-link
              [url]="donationUrl"
              label="Donations"
              icon="@tui.external-link"
              class="item-pointer"
            />
          }
        </div>
      </div>
    </div>
  `,
  styles: `
    .box-container {
      background-color: rgb(39 39 42);
      border-radius: 0.75rem;
      padding: 1.25rem 1.75rem;
    }

    .detail-container {
      display: grid;
      grid-auto-flow: row;
      grid-auto-columns: minmax(0, 1fr);

      & > * + * {
        border-top-width: 1px;
        border-bottom-width: 0;
        border-color: rgb(113 113 122);
      }
    }

    .item-pointer:hover {
      cursor: pointer;

      ::ng-deep label {
        cursor: pointer;
      }
    }

    .item-copy:hover {
      cursor: copy;

      ::ng-deep label {
        cursor: copy;
      }
    }

    .item-padding {
      padding: 0.75rem 0.25rem;
    }

    *,
    ::before,
    ::after {
      box-sizing: border-box;
      border-width: 0;
      border-style: solid;
      border-color: rgb(var(--tw-color-gray-200) / 1);
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [MarketplaceLinkComponent, i18nPipe],
})
export class MarketplaceLinksComponent {
  readonly copyService = inject(CopyService)
  readonly url =
    inject(ActivatedRoute).snapshot.queryParamMap.get('url') || undefined

  readonly pkg = input.required<MarketplacePkgBase>()
}
