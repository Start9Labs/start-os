import { Component, inject, Input } from '@angular/core'
import { TuiNotification } from '@taiga-ui/core'
import { ConfigService } from 'src/app/services/config.service'

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
          Services from this registry are packaged and maintained by the Start9
          team. If you experience an issue or have questions related to a
          service from this registry, one of our dedicated support staff will be
          happy to assist you.
        }
        @case ('info') {
          Services from this registry are packaged and maintained by members of
          the Start9 community.
          <strong>Install at your own risk</strong>
          . If you experience an issue or have a question related to a service
          in this marketplace, please reach out to the package developer for
          assistance.
        }
        @case ('warning') {
          Services from this registry are undergoing
          <strong>beta</strong>
          testing and may contain bugs.
          <strong>Install at your own risk</strong>
          .
        }
        @case ('error') {
          Services from this registry are undergoing
          <strong>alpha</strong>
          testing. They are expected to contain bugs and could damage your
          system.
          <strong>Install at your own risk</strong>
          .
        }
        @default {
          This is a Custom Registry. Start9 cannot verify the integrity or
          functionality of services from this registry, and they could damage
          your system.
          <strong>Install at your own risk</strong>
          .
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
  imports: [TuiNotification],
})
export class MarketplaceNotificationComponent {
  private readonly marketplace = inject(ConfigService).marketplace

  @Input() url = ''

  get status() {
    if (this.url === this.marketplace.start9) {
      return 'success'
    }

    if (this.url === this.marketplace.community) {
      return 'info'
    }

    if (this.url.includes('beta')) {
      return 'warning'
    }

    if (this.url.includes('alpha')) {
      return 'error'
    }

    return null
  }
}
