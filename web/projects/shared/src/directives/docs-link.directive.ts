import {
  computed,
  Directive,
  inject,
  InjectionToken,
  input,
} from '@angular/core'

export const VERSION = new InjectionToken<string>('VERSION')

@Directive({
  selector: '[docsLink]',
  host: {
    target: '_blank',
    rel: 'noreferrer',
    '[href]': 'url()',
  },
})
export class DocsLinkDirective {
  private readonly version = inject(VERSION)

  readonly href = input.required<string>()

  protected readonly url = computed(() => {
    const path = this.href()
    const relative = path.startsWith('/') ? path : `/${path}`
    return `https://docs.start9.com${relative}?os=${this.version}`
  })
}
