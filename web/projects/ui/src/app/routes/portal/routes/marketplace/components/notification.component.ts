import { Component, computed, input } from '@angular/core'
import { i18nPipe, knownRegistries, sameUrl } from '@start9labs/shared'
import { tuiAppearance, tuiNotificationOptionsProvider } from '@taiga-ui/core'

@Component({
  selector: '[tuiNotification][registry]',
  template: `
    @switch (status()) {
      @case ('positive') {
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
      @case ('negative') {
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
  `,
  providers: [tuiNotificationOptionsProvider({ icon: '', size: 'm' })],
  imports: [i18nPipe],
})
export class MarketplaceNotificationComponent {
  readonly registry = input<string | null>()

  protected readonly status = computed(() => {
    const { start9, community, start9Beta, communityBeta, start9Alpha } =
      knownRegistries

    if (sameUrl(this.registry(), start9)) {
      return 'positive'
    }

    if (sameUrl(this.registry(), community)) {
      return 'info'
    }

    if (
      sameUrl(this.registry(), start9Beta) ||
      sameUrl(this.registry(), communityBeta)
    ) {
      return 'warning'
    }

    if (sameUrl(this.registry(), start9Alpha)) {
      return 'negative'
    }

    return sameUrl(this.registry(), start9Alpha) ? 'negative' : null
  })

  protected readonly appearance = tuiAppearance(
    computed((status = this.status()) =>
      status === 'positive' || status === 'negative' ? status : 'info',
    ),
  )
}
