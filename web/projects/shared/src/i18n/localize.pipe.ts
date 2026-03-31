import { inject, Injectable, Pipe, PipeTransform } from '@angular/core'
import { i18nService } from './i18n.service'
import { I18N } from './i18n.providers'
import { T } from '@start9labs/start-sdk'

@Pipe({
  name: 'localize',
  pure: false,
})
@Injectable({ providedIn: 'root' })
export class LocalizePipe implements PipeTransform {
  private readonly i18nService = inject(i18nService)
  private readonly i18n = inject(I18N)

  transform(string: T.LocaleString): string {
    this.i18n() // read signal to trigger change detection on language switch
    return this.i18nService.localize(string)
  }
}
