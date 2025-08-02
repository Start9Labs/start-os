import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { Router } from '@angular/router'
import {
  MarketplaceAboutComponent,
  MarketplaceDependenciesComponent,
  MarketplaceFlavorsComponent,
  MarketplaceLinksComponent,
  MarketplacePackageHeroComponent,
  MarketplaceReleaseNotesComponent,
  MarketplaceVersionsComponent,
} from '@start9labs/marketplace'
import {
  DialogService,
  Exver,
  MARKDOWN,
  SharedPipesModule,
} from '@start9labs/shared'
import { TuiLoader } from '@taiga-ui/core'
import {
  BehaviorSubject,
  combineLatest,
  filter,
  map,
  shareReplay,
  startWith,
  switchMap,
  take,
} from 'rxjs'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { MarketplaceControlsComponent } from '../components/controls.component'

@Component({
  selector: 'marketplace-preview',
  template: `
    <div class="outer-container">
      <ng-content select="[slot=close]" />
      @if (pkg$ | async; as pkg) {
        <marketplace-package-hero [pkg]="pkg">
          <marketplace-controls [pkg]="pkg" />
        </marketplace-package-hero>
        <div class="inner-container">
          <marketplace-about [pkg]="pkg" (static)="onStatic()" />
          <marketplace-release-notes [pkg]="pkg" />
          @if (flavors$ | async; as flavors) {
            <marketplace-flavors [pkgs]="flavors" />
          }
          @if (!(pkg.dependencyMetadata | empty)) {
            <marketplace-dependencies [pkg]="pkg" (open)="open($event)" />
          }
          <marketplace-links [pkg]="pkg" />
          @if (versions$ | async; as versions) {
            <marketplace-versions
              [version]="pkg.version"
              [versions]="versions"
              (onVersion)="selectedVersion$.next($event)"
            />
          }
        </div>
      } @else {
        <tui-loader textContent="Loading" [style.height.%]="100" />
      }
    </div>
  `,
  styles: `
    :host {
      pointer-events: auto;
      overflow-y: auto;
      height: 100%;
      max-width: 100%;

      @media (min-width: 768px) {
        max-width: 30rem;
      }
    }

    .outer-container {
      display: flex;
      flex-direction: column;
      height: calc(100vh - var(--portal-header-height) - var(--bumper));
    }

    .inner-container {
      display: grid;
      grid-template-columns: repeat(1, minmax(0, 1fr));
      column-gap: 2rem;
    }

    .listing {
      font-size: 0.8rem;
      // @TODO theme
      color: #8059e5;
      font-weight: 600;
      display: flex;
      align-items: center;
      gap: 0.3rem;

      tui-icon {
        width: 0.8em;
        height: 0.8em;
      }
    }

    marketplace-versions {
      padding-bottom: 2rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    MarketplacePackageHeroComponent,
    MarketplaceDependenciesComponent,
    SharedPipesModule,
    TuiLoader,
    MarketplaceLinksComponent,
    MarketplaceFlavorsComponent,
    MarketplaceAboutComponent,
    MarketplaceControlsComponent,
    MarketplaceVersionsComponent,
    MarketplaceReleaseNotesComponent,
  ],
})
export class MarketplacePreviewComponent {
  private readonly dialog = inject(DialogService)
  private readonly exver = inject(Exver)
  private readonly router = inject(Router)
  private readonly marketplaceService = inject(MarketplaceService)

  readonly pkgId = input.required<string>()

  private readonly flavor$ = this.router.routerState.root.queryParamMap.pipe(
    map(paramMap => paramMap.get('flavor')),
    take(1),
  )

  readonly selectedVersion$ = new BehaviorSubject<string | null>(null)

  readonly pkg$ = combineLatest([this.selectedVersion$, this.flavor$]).pipe(
    switchMap(([version, flavor]) =>
      this.marketplaceService
        .getPackage$(this.pkgId(), version, flavor)
        .pipe(startWith(null)),
    ),
    shareReplay({ bufferSize: 1, refCount: false }),
  )

  readonly flavors$ = this.flavor$.pipe(
    switchMap(current =>
      this.marketplaceService.currentRegistry$.pipe(
        map(({ packages }) =>
          packages.filter(
            ({ id, flavor }) => id === this.pkgId() && flavor !== current,
          ),
        ),
        filter(p => p.length > 0),
      ),
    ),
  )

  readonly versions$ = combineLatest([
    this.pkg$.pipe(filter(Boolean)),
    this.flavor$,
  ]).pipe(
    map(([{ otherVersions, version }, flavor]) =>
      [
        version,
        ...Object.keys(otherVersions).filter(
          v => this.exver.getFlavor(v) === flavor,
        ),
      ].sort((a, b) => -1 * (this.exver.compareExver(a, b) || 0)),
    ),
  )

  open(id: string) {
    this.router.navigate([], { queryParams: { id } })
  }

  onStatic() {
    const content = this.pkg$.pipe(
      filter(Boolean),
      switchMap(pkg => this.marketplaceService.fetchStatic$(pkg)),
    )

    this.dialog
      .openComponent(MARKDOWN, {
        label: 'License',
        size: 'l',
        data: { content },
      })
      .subscribe()
  }
}
