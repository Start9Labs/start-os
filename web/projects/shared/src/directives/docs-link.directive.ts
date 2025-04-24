import {
  Directive,
  ElementRef,
  computed,
  effect,
  inject,
  input,
} from '@angular/core'
import { ConfigService } from '../../../ui/src/app/services/config.service'

@Directive({
  selector: '[docsLink]',
  standalone: true,
})
export class DocsLinkDirective {
  private readonly el = inject(ElementRef<HTMLAnchorElement>)
  private readonly OS_VERSION = inject(ConfigService).version

  readonly isDocs = input<boolean>(true)
  readonly href = input.required<string>()

  private readonly HOST = 'https://staging.docs.start9.com'

  private fullUrl = computed(() => {
    const path = this.href()
    const relative = path.startsWith('/') ? path : `/${path}`
    return `${this.HOST}${relative}?os=${this.OS_VERSION}`
  })

  private _ = effect(() => {
    const anchor = this.el.nativeElement
    anchor.target = '_blank'
    anchor.rel = 'noreferrer'

    if (this.isDocs()) {
      anchor.href = this.fullUrl()
    }
  })
}
