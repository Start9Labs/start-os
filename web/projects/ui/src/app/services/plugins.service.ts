import { inject, Injectable, INJECTOR, Injector, signal } from '@angular/core'
import { toObservable } from '@angular/core/rxjs-interop'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { TUI_WINDOW_SIZE } from '@taiga-ui/cdk'
import { TUI_MEDIA } from '@taiga-ui/core'
import { combineLatest, map, Observable } from 'rxjs'

@Injectable({ providedIn: 'root' })
export class PluginsService extends Observable<DOMRect> {
  public readonly enabled = signal(false)
  public readonly size = signal(100)

  // @ts-expect-error triggering TUI_WINDOW_SIZE default factory
  private readonly window = Injector.create({
    parent: inject(INJECTOR),
    providers: [{ provide: TUI_WINDOW_SIZE }],
  }).get(TUI_WINDOW_SIZE)

  private readonly media = inject(TUI_MEDIA)
  private readonly stream = inject(WA_IS_MOBILE)
    ? this.window
    : combineLatest([
        this.window,
        toObservable(this.size),
        toObservable(this.enabled),
      ]).pipe(
        map(([window, size, enabled]) =>
          window.width < this.media.mobile || !enabled
            ? window
            : { ...window, width: 320 + (window.width - 640) * (size / 100) },
        ),
      )

  constructor() {
    super(subscriber => this.stream.subscribe(subscriber))
  }
}
