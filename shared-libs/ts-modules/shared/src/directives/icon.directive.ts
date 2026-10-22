import { computed, Directive, Signal, signal } from '@angular/core'

export const FALLBACK_ICON = 'assets/img/service-icons/fallback.png'

/** An icon `<img>` whose source arrives asynchronously; null while loading. */
@Directive({
  host: {
    alt: '',
    '[attr.src]': 'src() && failed() === src() ? fallback : src()',
    '(error)': 'failed.set(src())',
  },
})
export abstract class IconDirective {
  protected abstract readonly src: Signal<string | null>
  protected readonly fallback = FALLBACK_ICON
  protected readonly failed = signal<string | null>(null)

  readonly loading = computed(() => !this.src())
}
