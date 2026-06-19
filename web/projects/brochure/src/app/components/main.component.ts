import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { toObservable, toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute, NavigationStart, Router } from '@angular/router'
import { MarketplaceComponent, MarketplacePkg } from '@start9labs/marketplace'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { skip, takeUntil } from 'rxjs'
import { filter } from 'rxjs/operators'
import { PackageComponent } from 'src/app/components/package.component'
import { MarketplaceRegistryComponent } from 'src/app/components/registry.component'
import { MarketplaceTileComponent } from 'src/app/components/tile.component'
import { MarketplaceService } from 'src/app/services/marketplace.service'

@Component({
  template: `
    <marketplace
      [registry]="registry()"
      [(category)]="category"
      [(query)]="query"
    >
      <marketplace-registry />
      <a
        tuiButton
        iconStart="@tui.shopping-cart"
        appearance="outline"
        target="_blank"
        rel="noreferrer"
        href="https://store.start9.com"
      >
        Get a Start9 Server
      </a>
      <ng-template let-pkg>
        <button
          type="button"
          [marketplaceTile]="pkg"
          (click)="navigate(pkg)"
        ></button>
      </ng-template>
    </marketplace>
  `,
  styles: `
    marketplace {
      height: 100%;

      ::ng-deep .t-nav-content {
        display: grid;
      }

      [tuiButton] {
        order: 1;
        justify-content: flex-start;
        gap: 0.625rem;
        font-weight: bold;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    MarketplaceComponent,
    MarketplaceTileComponent,
    MarketplaceRegistryComponent,
    TuiButton,
  ],
})
export class MainComponent {
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)
  private readonly service = inject(MarketplaceService)
  private readonly dialogs = inject(TuiResponsiveDialogService)

  protected readonly category = signal('all')
  protected readonly query = signal('')
  protected readonly registry = toSignal(this.service.getRegistry$())
  protected readonly pkgId = this.route.queryParams.subscribe(
    ({ id, flavor }) => {
      if (id) {
        this.open(id, flavor)
      }
    },
  )

  protected readonly sub = toObservable(this.category)
    .pipe(skip(1))
    .subscribe(() => this.router.navigate(['']))

  protected navigate({ id, flavor }: MarketplacePkg) {
    this.router.navigate(['/'], { queryParams: { id, flavor } })
  }

  protected open(id: string, flavor: string) {
    this.dialogs
      .open(new PolymorpheusComponent(PackageComponent), {
        data: { id, flavor },
        required: true,
      })
      .pipe(
        takeUntil(
          this.router.events.pipe(filter(e => e instanceof NavigationStart)),
        ),
      )
      .subscribe({ error: () => this.router.navigate(['']) })
  }
}
