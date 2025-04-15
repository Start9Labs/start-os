import { inject, Pipe, PipeTransform } from '@angular/core'
import ENGLISH from './dictionaries/english'
import { I18N, i18nKey } from './i18n.providers'

@Pipe({
  standalone: true,
  name: 'i18n',
  pure: false,
})
export class i18nPipe implements PipeTransform {
  private readonly i18n = inject(I18N)

  transform(path: i18nKey): string {
    return this.i18n()?.[ENGLISH[path]] || path
  }
}
