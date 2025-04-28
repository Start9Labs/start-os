import { Component, Input } from '@angular/core'
import { i18nPipe, knownRegistries, sameUrl } from '@start9labs/shared'
import { TuiNotification } from '@taiga-ui/core'

@Component({
  standalone: true,
  selector: 'marketplace-notification',
  template: `
    <tui-notification
      [appearance]="status || 'warning'"
      icon=""
      class="notification-wrapper"
    >
      @switch (status) {
        @case ('success') {
          {{
            'Services from this registry are packaged and maintained by the Start9 team. If you experience an issue or have questions related to a service from this registry, one of our dedicated support staff will be happy to assist you.'
              | i18n
          }}
        }
        @case ('info') {
          {{
            'Services from this registry are packaged and maintained by members of the Start9 community. Install at your own risk. If you experience an issue or have a question related to a service in this marketplace, please reach out to the package developer for assistance.'
              | i18n
          }}
        }
        @case ('warning') {
          {{
            'Services from this registry are undergoing beta testing and may contain bugs. Install at your own risk.'
              | i18n
          }}
        }
        @case ('error') {
          {{
            'Services from this registry are undergoing alpha testing. They are expected to contain bugs and could damage your system. Install at your own risk.'
              | i18n
          }}
        }
        @default {
          {{
            'This is a Custom Registry. Start9 cannot verify the integrity or functionality of services from this registry, and they could damage your system. Install at your own risk.'
              | i18n
          }}
        }
      }
    </tui-notification>
  `,
  styles: [
    `
      .notification-wrapper {
        margin: 1rem;
        pointer-events: none;
      }
    `,
  ],
  imports: [TuiNotification, i18nPipe],
})
export class MarketplaceNotificationComponent {
  @Input() url = ''

  get status() {
    const { start9, community, start9Beta, start9Alpha } = knownRegistries

    if (sameUrl(this.url, start9)) {
      return 'success'
    }

    if (sameUrl(this.url, community)) {
      return 'info'
    }

    if (sameUrl(this.url, start9Beta)) {
      return 'warning'
    }

    if (sameUrl(this.url, start9Alpha)) {
      return 'error'
    }

    return null
  }
}
