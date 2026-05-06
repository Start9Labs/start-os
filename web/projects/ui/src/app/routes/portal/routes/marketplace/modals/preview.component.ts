import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
  signal,
} from '@angular/core'
import { Router } from '@angular/router'
import {
  MarketplaceAboutComponent,
  MarketplaceDependenciesComponent,
  MarketplaceFlavorsComponent,
  MarketplaceLinksComponent,
  MarketplacePackageHeroComponent,
  MarketplacePkg,
  MarketplaceReleaseNotesComponent,
} from '@start9labs/marketplace'
import {
  DialogService,
  EmptyPipe,
  Exver,
  i18nPipe,
  MARKDOWN,
} from '@start9labs/shared'
import { TuiLoader, TuiNotification } from '@taiga-ui/core'
import {
  BehaviorSubject,
  combineLatest,
  filter,
  map,
  shareReplay,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs'
import { toSignal } from '@angular/core/rxjs-interop'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { MarketplaceControlsComponent } from '../components/controls.component'

@Component({
  selector: 'marketplace-preview',
  template: `
    <div class="outer-container">
      <ng-content select="[slot=close]" />
      @if (noMatch()) {
        <div tuiNotification appearance="negative" class="no-match">
          <strong>{{ 'No compatible version available' | i18n }}</strong>
          <span>{{ 'Requires' | i18n }}: {{ targetRange() }}</span>
        </div>
      } @else if (pkg$ | async; as pkg) {
        <marketplace-package-hero [pkg]="pkg">
          <marketplace-controls [pkg]="pkg" />
        </marketplace-package-hero>
        <div class="inner-container">
          <marketplace-about
            [pkg]="pkg"
            [versions]="versions$ | async"
            (static)="onStatic()"
            (onVersion)="selectedVersion$.next($event)"
          />
          <marketplace-release-notes [pkg]="pkg" />
          @if (flavors$ | async; as flavors) {
            <marketplace-flavors [pkgs]="flavors" />
          }
          @if (!(pkg.dependencyMetadata | empty)) {
            <marketplace-dependencies [pkg]="pkg" (open)="open($event)" />
          }
          <marketplace-links [pkg]="pkg" />
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
      height: calc(100dvh - var(--portal-header-height) - var(--bumper));
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

    .no-match {
      margin: 1rem;
      display: flex;
      flex-direction: column;
      gap: 0.25rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    MarketplacePackageHeroComponent,
    MarketplaceDependenciesComponent,
    EmptyPipe,
    TuiLoader,
    TuiNotification,
    MarketplaceLinksComponent,
    MarketplaceFlavorsComponent,
    MarketplaceAboutComponent,
    MarketplaceControlsComponent,
    MarketplaceReleaseNotesComponent,
    i18nPipe,
  ],
})
export class MarketplacePreviewComponent {
  private readonly dialog = inject(DialogService)
  private readonly exver = inject(Exver)
  private readonly router = inject(Router)
  private readonly marketplaceService = inject(MarketplaceService)

  readonly pkgId = input.required<string>()

  private readonly params$ = this.router.routerState.root.queryParamMap.pipe(
    take(1),
    shareReplay(1),
  )

  private readonly params = toSignal(this.params$)

  private readonly flavor$ = this.params$.pipe(map(p => p.get('flavor')))

  readonly targetRange = computed(() => {
    const params = this.params()
    return params?.get('requirePkg') === this.pkgId()
      ? params.get('requireRange')
      : null
  })

  readonly selectedVersion$ = new BehaviorSubject<string | null>(null)

  readonly noMatch = signal(false)

  readonly pkg$ = combineLatest([this.selectedVersion$, this.flavor$]).pipe(
    switchMap(([version, flavor]) =>
      this.marketplaceService.getPackage$(this.pkgId(), version, flavor).pipe(
        tap(pkg => {
          if (!pkg || version) return
          const range = this.targetRange()
          if (!range || this.satisfiesWithAliases(pkg, range)) return
          const flv = this.exver.getFlavor(pkg.version)
          const candidate = Object.keys(pkg.otherVersions || {})
            .filter(
              v =>
                this.exver.getFlavor(v) === flv &&
                this.exver.satisfies(v, range),
            )
            .sort((a, b) => -1 * (this.exver.compareExver(a, b) || 0))[0]
          if (candidate) {
            this.selectedVersion$.next(candidate)
          } else {
            this.noMatch.set(true)
          }
        }),
        filter(pkg => {
          if (!pkg) return false
          if (version) return true
          const range = this.targetRange()
          return !range || this.satisfiesWithAliases(pkg, range)
        }),
        startWith<MarketplacePkg | null>(null),
      ),
    ),
    shareReplay({ bufferSize: 1, refCount: false }),
  )

  private satisfiesWithAliases(pkg: MarketplacePkg, range: string): boolean {
    if (this.exver.satisfies(pkg.version, range)) return true
    return (pkg.satisfies || []).some(v => this.exver.satisfies(v, range))
  }

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
    map(([pkg, flavor]) => {
      const range = this.targetRange()
      const others = Object.keys(pkg.otherVersions).filter(
        v => this.exver.getFlavor(v) === flavor,
      )
      return [pkg.version, ...others]
        .filter(v => {
          if (!range) return true
          if (v === pkg.version) return this.satisfiesWithAliases(pkg, range)
          return this.exver.satisfies(v, range)
        })
        .sort((a, b) => -1 * (this.exver.compareExver(a, b) || 0))
    }),
  )

  open(id: string) {
    // @TODO re-engage when button can link to root with search QP
    // this.router.navigate([], { queryParams: { id } })
  }

  onStatic() {
    this.dialog
      .openComponent(MARKDOWN, {
        label: 'License',
        size: 'l',
        data: this.pkg$.pipe(
          filter(Boolean),
          switchMap(pkg => this.marketplaceService.fetchStatic$(pkg)),
        ),
      })
      .subscribe()
  }
}
