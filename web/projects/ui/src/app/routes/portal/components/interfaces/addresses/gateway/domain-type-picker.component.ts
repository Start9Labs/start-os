import { ChangeDetectionStrategy, Component } from '@angular/core'
import { DocsLinkDirective, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiIcon, TuiLink } from '@taiga-ui/core'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

export type DomainType = 'public' | 'private'

@Component({
  selector: 'domain-type-picker',
  template: `
    <div class="cards">
      <section class="card">
        <div class="head">
          <tui-icon icon="@tui.globe" />
          <h3>{{ 'Public Domain' | i18n }}</h3>
        </div>
        <p class="tagline">
          {{
            'A clearnet domain reachable from anywhere on the Internet.' | i18n
          }}
        </p>

        <h4>{{ 'Best for' | i18n }}</h4>
        <ul>
          <li>{{ 'Public websites & APIs' | i18n }}</li>
          <li>{{ 'Remote access without a VPN' | i18n }}</li>
          <li>{{ 'Standard HTTPS — nothing extra to trust' | i18n }}</li>
        </ul>

        <h4>{{ 'Requires' | i18n }}</h4>
        <ul>
          <li>{{ 'A domain you own' | i18n }}</li>
          <li>{{ 'DNS record → your gateway’s public IP' | i18n }}</li>
          <li>{{ 'Port forwarding (usually 443)' | i18n }}</li>
        </ul>

        <footer>
          <a
            tuiLink
            iconEnd="@tui.book-open-text"
            docsLink
            path="/start-os/clearnet.html"
          >
            {{ 'Learn more' | i18n }}
          </a>
          <button tuiButton size="m" (click)="select('public')">
            {{ 'Add Public Domain' | i18n }}
          </button>
        </footer>
      </section>

      <section class="card">
        <div class="head">
          <tui-icon icon="@tui.house" />
          <h3>{{ 'Private Domain' | i18n }}</h3>
        </div>
        <p class="tagline">
          {{
            'A custom domain that only works on your LAN or over VPN.' | i18n
          }}
        </p>

        <h4>{{ 'Best for' | i18n }}</h4>
        <ul>
          <li>{{ 'Personal or shared private access' | i18n }}</li>
          <li>{{ 'Services never exposed to the Internet' | i18n }}</li>
          <li>{{ 'Any domain — real or made-up.' | i18n }}</li>
        </ul>

        <h4>{{ 'Requires' | i18n }}</h4>
        <ul>
          <li>{{ 'This server as your gateway’s DNS server' | i18n }}</li>
          <li>{{ 'Client devices connected to the same LAN' | i18n }}</li>
          <li>{{ 'Client devices to trust your Root CA' | i18n }}</li>
        </ul>

        <footer>
          <a
            tuiLink
            iconEnd="@tui.book-open-text"
            docsLink
            path="/start-os/private-domains.html"
          >
            {{ 'Learn more' | i18n }}
          </a>
          <button tuiButton size="m" (click)="select('private')">
            {{ 'Add Private Domain' | i18n }}
          </button>
        </footer>
      </section>
    </div>
  `,
  styles: `
    :host {
      display: block;
    }

    .cards {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 1rem;
    }

    .card {
      display: flex;
      flex-direction: column;
      padding: 1.25rem;
      border-radius: var(--tui-radius-l);
      background: var(--tui-background-neutral-1);
      border: 1px solid var(--tui-border-normal);
    }

    .head {
      display: flex;
      align-items: center;
      gap: 0.625rem;
      margin-bottom: 0.5rem;

      tui-icon {
        font-size: 1.75rem;
        color: var(--tui-text-primary);
      }

      h3 {
        margin: 0;
        font: var(--tui-font-heading-6);
      }
    }

    .tagline {
      margin: 0 0 0.5rem;
      font-weight: 600;
      color: var(--tui-text-primary);
    }

    h4 {
      margin: 1rem 0 0.375rem;
      font: var(--tui-font-text-s);
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      color: var(--tui-text-secondary);
    }

    p {
      margin: 0;
      color: var(--tui-text-secondary);
      line-height: 1.5;
    }

    ul {
      margin: 0;
      padding-inline-start: 1.1rem;
      color: var(--tui-text-secondary);
      line-height: 1.5;

      li + li {
        margin-top: 0.25rem;
      }
    }

    footer {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 0.75rem;
      margin-top: auto;
      padding-top: 1.25rem;

      a[tuiLink] {
        color: var(--tui-text-primary);
      }
    }

    :host-context(tui-root._mobile) {
      .cards {
        grid-template-columns: 1fr;
      }

      footer {
        flex-wrap: wrap;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [DocsLinkDirective, TuiButton, TuiIcon, TuiLink, i18nPipe],
})
export class DomainTypePickerComponent {
  readonly context = injectContext<TuiDialogContext<DomainType>>()

  select(type: DomainType) {
    this.context.completeWith(type)
  }
}

export const DOMAIN_TYPE_PICKER = new PolymorpheusComponent(
  DomainTypePickerComponent,
)
