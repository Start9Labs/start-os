import { inject, Injectable, Pipe, PipeTransform } from '@angular/core'
import { i18nService } from './i18n.service'
import { T } from '@start9labs/start-sdk'

@Pipe({
  name: 'localize',
  pure: false,
})
@Injectable({ providedIn: 'root' })
export class LocalizePipe implements PipeTransform {
  private readonly i18nService = inject(i18nService)

  transform(string: T.LocaleString): string {
    return this.i18nService.localize(string)
  }
}
