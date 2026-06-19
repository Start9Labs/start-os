import {
  provideHttpClient,
  withInterceptorsFromDi,
  withXhr,
} from '@angular/common/http'
import { ApplicationConfig } from '@angular/core'
import { provideRouter, withInMemoryScrolling } from '@angular/router'
import { I18N_PROVIDERS, i18nPipe, RELATIVE_URL } from '@start9labs/shared'
import { provideTaiga } from '@taiga-ui/core'
import { ApiService } from 'src/app/services/api.service'
import { LiveApiService } from 'src/app/services/live-api.service'
import { MockApiService } from 'src/app/services/mock-api.service'
import { ROUTES } from 'src/app/app.routes'
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
  ],
}
