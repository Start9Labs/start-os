import { ChangeDetectionStrategy, Component } from '@angular/core'
import { DocsLinkDirective, i18nPipe } from '@start9labs/shared'
import {
  TuiButton,
  TuiDialogContext,
  TuiIcon,
  TuiLink,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiCard, TuiHeader, TuiList } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

export type DomainType = 'public' | 'private'

@Component({
  selector: 'domain-type-picker',
  template: `
    <section tuiCardLarge="compact" appearance="secondary-grayscale">
      <header tuiHeader="body-l">
        <tui-icon icon="@tui.globe" />
        <h3 tuiTitle>{{ 'Public Domain' | i18n }}</h3>
      </header>
      <div>
        {{
          'A clearnet domain reachable from anywhere on the Internet.' | i18n
        }}
      </div>

      <h4 tuiHeader="body-s">{{ 'Best for' | i18n }}</h4>
      <ul tuiList="s">
        <li>{{ 'Public websites & APIs' | i18n }}</li>
        <li>{{ 'Remote access without a VPN' | i18n }}</li>
        <li>{{ 'Standard HTTPS — nothing extra to trust' | i18n }}</li>
      </ul>

      <h4 tuiHeader="body-s">{{ 'Requires' | i18n }}</h4>
      <ul tuiList="s">
        <li>{{ 'A domain you own' | i18n }}</li>
        <li>{{ 'DNS record → your gateway’s public IP' | i18n }}</li>
        <li>{{ 'Port forwarding (usually 443)' | i18n }}</li>
      </ul>

      <footer>
        <a
          tuiLink
          appearance=""
          iconEnd="@tui.book-open-text"
          docsLink
          path="/start-os/clearnet.html"
          [textContent]="'Learn more' | i18n"
        ></a>
        <button tuiButton size="m" (click)="context.completeWith('public')">
          {{ 'Add Public Domain' | i18n }}
        </button>
      </footer>
    </section>

    <section tuiCardLarge="compact" appearance="secondary-grayscale">
      <header tuiHeader="body-l">
        <tui-icon icon="@tui.house" />
        <h3 tuiTitle>{{ 'Private Domain' | i18n }}</h3>
      </header>
      <div>
        {{ 'A custom domain that only works on your LAN or over VPN.' | i18n }}
      </div>

      <h4 tuiHeader="body-s">{{ 'Best for' | i18n }}</h4>
      <ul tuiList="s">
        <li>{{ 'Personal or shared private access' | i18n }}</li>
        <li>{{ 'Services never exposed to the Internet' | i18n }}</li>
        <li>{{ 'Any domain — real or made-up.' | i18n }}</li>
      </ul>

      <h4 tuiHeader="body-s">{{ 'Requires' | i18n }}</h4>
      <ul tuiList="s">
        <li>{{ 'This server as your gateway’s DNS server' | i18n }}</li>
        <li>{{ 'Client devices connected to the same LAN' | i18n }}</li>
        <li>{{ 'Client devices to trust your Root CA' | i18n }}</li>
      </ul>

      <footer>
        <a
          tuiLink
          appearance=""
          iconEnd="@tui.book-open-text"
          docsLink
          path="/start-os/private-domains.html"
          [textContent]="'Learn more' | i18n"
        ></a>
        <button tuiButton size="m" (click)="context.completeWith('private')">
          {{ 'Add Private Domain' | i18n }}
        </button>
      </footer>
    </section>
  `,
  styles: `
    :host {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 1rem;
    }

    tui-icon {
      font-size: 1.75rem;
    }

    [tuiCardLarge] {
      box-shadow: inset 0 0 0 1px var(--tui-border-normal);
      font-weight: 600;
    }

    [tuiHeader] {
      font-weight: bold;
      gap: 0.75rem;
    }

    h4 {
      text-transform: uppercase;
      color: var(--tui-text-secondary);
    }

    ul {
      margin-block: 0;
      color: var(--tui-text-secondary);
    }

    footer {
      display: flex;
      align-items: center;
      flex-wrap: wrap;
      justify-content: space-between;
      gap: 0.75rem;
    }

    :host-context(tui-root._mobile) {
      grid-template-columns: 1fr;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    DocsLinkDirective,
    TuiButton,
    TuiIcon,
    TuiLink,
    i18nPipe,
    TuiCard,
    TuiHeader,
    TuiTitle,
    TuiList,
  ],
})
export class DomainTypePickerComponent {
  protected readonly context = injectContext<TuiDialogContext<DomainType>>()
}

export const DOMAIN_TYPE_PICKER = new PolymorpheusComponent(
  DomainTypePickerComponent,
)
