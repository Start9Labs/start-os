import { inject, Pipe, PipeTransform } from '@angular/core'
import { I18N, i18n } from './i18n.providers'

type DeepKeyOf<T> = {
  [K in keyof T & string]: T[K] extends {}
    ? T[K] extends string
      ? K
      : `${K}.${DeepKeyOf<T[K]>}`
    : never
}[keyof T & string]

@Pipe({
  standalone: true,
  name: 'i18n',
  pure: false,
})
export class i18nPipe implements PipeTransform {
  private readonly i18n = inject(I18N)

  transform(path: DeepKeyOf<i18n>): string {
    return path.split('.').reduce((acc, part) => acc[part], this.i18n() as any)
  }
}
