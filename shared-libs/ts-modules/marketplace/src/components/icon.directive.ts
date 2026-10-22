import { computed, Directive, inject, input } from '@angular/core'
import { toObservable, toSignal } from '@angular/core/rxjs-interop'
import { FALLBACK_ICON, IconDirective } from '@start9labs/shared'
import { catchError, defer, of, startWith, switchMap } from 'rxjs'

import { AbstractMarketplaceService } from '../services/abstract-marketplace.service'
import { MarketplacePkgBase } from '../types'
import { registryIconUrl } from '../util/icon'

@Directive({
  selector: 'img[marketplaceIcon]',
  exportAs: 'marketplaceIcon',
})
export class MarketplaceIconDirective extends IconDirective {
  private readonly marketplace = inject(AbstractMarketplaceService, {
    optional: true,
  })
  private readonly current = toSignal(
    this.marketplace?.currentRegistryUrl$ || of(null),
  )

  readonly marketplaceIcon = input.required<MarketplacePkgBase>()
  readonly dependency = input<string>()
  readonly registry = input<string>()

  protected readonly src = toSignal(
    toObservable(
      computed(() => ({
        pkg: this.marketplaceIcon(),
        dependency: this.dependency(),
        registry: this.registry() || this.current(),
      })),
    ).pipe(
      switchMap(({ pkg, dependency, registry }) =>
        defer(() => {
          const inline = dependency
            ? pkg.dependencyMetadata[dependency]?.icon
            : pkg.icon

          if (inline || !registry) return of(inline || FALLBACK_ICON)

          return this.marketplace
            ? this.marketplace.fetchIcon$(
                registry,
                pkg.id,
                pkg.version,
                dependency,
              )
            : of(registryIconUrl(registry, pkg.id, pkg.version, dependency))
        }).pipe(
          catchError(() => of(FALLBACK_ICON)),
          startWith(null),
        ),
      ),
    ),
    { initialValue: null },
  )
}
