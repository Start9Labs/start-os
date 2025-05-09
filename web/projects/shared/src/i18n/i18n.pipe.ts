import { inject, Injectable, Pipe, PipeTransform } from '@angular/core'
import { ENGLISH } from './dictionaries/en'
import { I18N, i18nKey } from './i18n.providers'

@Pipe({
  standalone: true,
  name: 'i18n',
  pure: false,
})
@Injectable({ providedIn: 'root' })
export class i18nPipe implements PipeTransform {
  private readonly i18n = inject(I18N)

  // @TODO uncomment to make sure translations are present
  transform(englishKey: string | null | undefined): string | undefined {
    // transform(englishKey: i18nKey | null | undefined): string | undefined {
    return englishKey
      ? this.i18n()?.[ENGLISH[englishKey as i18nKey]] || englishKey
      : undefined
  }
}
