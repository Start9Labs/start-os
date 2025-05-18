import {
  computed,
  Directive,
  inject,
  InjectionToken,
  input,
} from '@angular/core'

const HOST = 'https://staging.docs.start9.com'
export const VERSION = new InjectionToken<string>('VERSION')

@Directive({
  selector: '[docsLink]',
  standalone: true,
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
    return `${HOST}${relative}?os=${this.version}`
  })
}
