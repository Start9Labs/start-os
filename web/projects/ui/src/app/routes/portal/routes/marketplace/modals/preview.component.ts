import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  TemplateRef,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { Router } from '@angular/router'
import {
  AboutModule,
  AdditionalModule,
  FlavorsComponent,
  MarketplaceAdditionalItemComponent,
  MarketplaceDependenciesComponent,
  MarketplacePackageHeroComponent,
  MarketplacePkg,
} from '@start9labs/marketplace'
import {
  DialogService,
  Exver,
  i18nPipe,
  SharedPipesModule,
} from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiLoader } from '@taiga-ui/core'
import { TuiRadioList } from '@taiga-ui/kit'
import {
  BehaviorSubject,
  combineLatest,
  filter,
  map,
  startWith,
  switchMap,
} from 'rxjs'
import { MarketplaceService } from 'src/app/services/marketplace.service'

@Component({
  selector: 'marketplace-preview',
  template: `
    <div class="outer-container">
      <ng-content select="[slot=close]" />
      @if (pkg$ | async; as pkg) {
        <marketplace-package-hero [pkg]="pkg">
          <ng-content select="[slot=controls]" />
        </marketplace-package-hero>
        <div class="inner-container">
          @if (flavors$ | async; as flavors) {
            <marketplace-flavors [pkgs]="flavors" />
          }
          <marketplace-about [pkg]="pkg" />
          @if (!(pkg.dependencyMetadata | empty)) {
            <marketplace-dependencies [pkg]="pkg" (open)="open($event)" />
          }
          <marketplace-additional [pkg]="pkg" (static)="onStatic($event)">
            @if (versions$ | async; as versions) {
              <marketplace-additional-item
                (click)="versions.length ? selectVersion(pkg, version) : 0"
                [data]="
                  versions.length
                    ? 'Click to view all versions'
                    : 'No other versions'
                "
                label="All versions"
                icon="@tui.chevron-right"
                class="versions"
              />
              <ng-template
                #version
                let-data="data"
                let-completeWith="completeWith"
              >
                <tui-radio-list [items]="versions" [(ngModel)]="data.value" />
                <footer class="buttons">
                  <button
                    tuiButton
                    appearance="secondary"
                    (click)="completeWith(null)"
                  >
                    {{ 'Cancel' | i18n }}
                  </button>
                  <button
                    tuiButton
                    appearance="secondary"
                    (click)="completeWith(data.value)"
                  >
                    {{ 'Ok' | i18n }}
                  </button>
                </footer>
              </ng-template>
            }
          </marketplace-additional>
        </div>
      } @else {
        <tui-loader class="loading" textContent="Loading" />
      }
    </div>
  `,
  styles: [
    `
      :host {
        pointer-events: auto;
      }

      .outer-container {
        display: flex;
        flex-direction: column;
        padding: 1.75rem;
        min-width: 100%;
        height: calc(100vh - var(--portal-header-height) - var(--bumper));
        margin-top: 5rem;

        @media (min-width: 768px) {
          margin-top: 0;
        }
      }

      .inner-container {
        display: grid;
        grid-template-columns: repeat(1, minmax(0, 1fr));
        column-gap: 2rem;
      }

      .listing {
        font-size: 0.9rem;
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

      .versions {
        border: 0;
        border-top-width: 1px;
        border-bottom-width: 1px;
        border-color: rgb(113 113 122);
        border-style: solid;
        cursor: pointer;
        ::ng-deep label {
          cursor: pointer;
        }
      }

      .loading {
        min-width: 30rem;
        height: 100%;
        place-self: center;
      }

      marketplace-additional {
        padding-bottom: 2rem;
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    MarketplacePackageHeroComponent,
    MarketplaceDependenciesComponent,
    MarketplaceAdditionalItemComponent,
    TuiButton,
    AdditionalModule,
    AboutModule,
    SharedPipesModule,
    FormsModule,
    TuiRadioList,
    TuiLoader,
    FlavorsComponent,
    i18nPipe,
  ],
})
export class MarketplacePreviewComponent {
  @Input({ required: true })
  pkgId!: string

  private readonly dialog = inject(DialogService)
  private readonly exver = inject(Exver)
  private readonly router = inject(Router)
  private readonly marketplaceService = inject(MarketplaceService)
  private readonly version$ = new BehaviorSubject<string | null>(null)
  private readonly flavor$ = this.router.routerState.root.queryParamMap.pipe(
    map(paramMap => paramMap.get('flavor')),
  )

  readonly pkg$ = combineLatest([this.version$, this.flavor$]).pipe(
    switchMap(([version, flavor]) =>
      this.marketplaceService
        .getPackage$(this.pkgId, version, flavor)
        .pipe(startWith(null)),
    ),
  )

  readonly flavors$ = this.flavor$.pipe(
    switchMap(current =>
      this.marketplaceService.getRegistry$().pipe(
        map(({ packages }) =>
          packages.filter(
            ({ id, flavor }) => id === this.pkgId && flavor !== current,
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
    map(([{ otherVersions }, flavor]) =>
      Object.keys(otherVersions)
        .filter(v => this.exver.getFlavor(v) === flavor)
        .sort((a, b) => -1 * (this.exver.compareExver(a, b) || 0)),
    ),
  )

  open(id: string) {
    this.router.navigate([], { queryParams: { id } })
  }

  onStatic(type: 'License' | 'Instructions') {
    // @TODO Alex need to display License or Instructions. This requires an API request, check out next/minor
  }

  selectVersion(
    { version }: MarketplacePkg,
    template: TemplateRef<TuiDialogContext>,
  ) {
    this.dialog
      .openComponent<string>(template, {
        label: 'Versions',
        size: 's',
        data: {
          value: version,
        },
      })
      .pipe(filter(Boolean))
      .subscribe(version => this.version$.next(version))
  }
}
