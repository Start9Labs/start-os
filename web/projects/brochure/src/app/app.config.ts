import {
  provideHttpClient,
  withInterceptorsFromDi,
  withXhr,
} from '@angular/common/http'
import { ApplicationConfig } from '@angular/core'
import { provideRouter, withInMemoryScrolling } from '@angular/router'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { I18N_PROVIDERS, i18nPipe, RELATIVE_URL } from '@start9labs/shared'
import { tuiProvide } from '@taiga-ui/cdk'
import { provideTaiga } from '@taiga-ui/core'
import { ROUTES } from 'src/app/app.routes'
import { ApiService } from 'src/app/services/api.service'
import { LiveApiService } from 'src/app/services/live-api.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { MockApiService } from 'src/app/services/mock-api.service'
import { environment } from 'src/environments/environment'

export const APP_CONFIG: ApplicationConfig = {
  providers: [
    provideHttpClient(withXhr(), withInterceptorsFromDi()),
    provideRouter(
      ROUTES,
      withInMemoryScrolling({ scrollPositionRestoration: 'enabled' }),
    ),
    provideTaiga({ mode: 'dark' }),
    I18N_PROVIDERS,
    i18nPipe,
    {
      provide: RELATIVE_URL,
      useValue: `/rpc/v0`,
    },
    {
      provide: ApiService,
      useClass: environment.production ? LiveApiService : MockApiService,
    },
    tuiProvide(AbstractMarketplaceService, MarketplaceService),
  ],
}
