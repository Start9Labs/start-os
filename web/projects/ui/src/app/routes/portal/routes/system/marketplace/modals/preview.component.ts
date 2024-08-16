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
  AbstractMarketplaceService,
  AdditionalModule,
  MarketplaceAdditionalItemComponent,
  MarketplaceDependenciesComponent,
  MarketplacePackageHeroComponent,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { Exver, SharedPipesModule } from '@start9labs/shared'
import {
  TuiButton,
  TuiDialogContext,
  TuiDialogService,
  TuiIcon,
  TuiLoader,
} from '@taiga-ui/core'
import { TuiRadioList, TuiStringifyContentPipe } from '@taiga-ui/kit'
import { BehaviorSubject, filter, switchMap, tap } from 'rxjs'

@Component({
  selector: 'marketplace-preview',
  template: `
    <div class="outer-container">
      <ng-content select="[slot=close]" />
      @if (pkg$ | async; as pkg) {
        @if (loading$ | async) {
          <tui-loader class="loading" textContent="Loading" />
        } @else {
          <marketplace-package-hero [pkg]="pkg">
            <ng-content select="[slot=controls]" />
          </marketplace-package-hero>
          <div class="inner-container">
            <marketplace-about [pkg]="pkg" />
            @if (!(pkg.dependencyMetadata | empty)) {
              <marketplace-dependencies
                [pkg]="pkg"
                (open)="open($event)"
              ></marketplace-dependencies>
            }
            <marketplace-additional [pkg]="pkg">
              <marketplace-additional-item
                (click)="presentAlertVersions(pkg, version)"
                data="Click to view all versions"
                label="All versions"
                icon="@tui.chevron-right"
                class="versions"
              ></marketplace-additional-item>
              <ng-template
                #version
                let-data="data"
                let-completeWith="completeWith"
              >
                <tui-radio-list [items]="data.items" [(ngModel)]="data.value" />
                <footer class="buttons">
                  <button
                    tuiButton
                    appearance="secondary"
                    (click)="completeWith(null)"
                  >
                    Cancel
                  </button>
                  <button
                    tuiButton
                    appearance="secondary"
                    (click)="loading$.next(true); completeWith(data.value)"
                  >
                    Ok
                  </button>
                </footer>
              </ng-template>
            </marketplace-additional>
          </div>
        }
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
    TuiStringifyContentPipe,
    TuiRadioList,
    TuiLoader,
    TuiIcon,
  ],
})
export class MarketplacePreviewComponent {
  @Input({ required: true })
  pkgId!: string

  readonly loading$ = new BehaviorSubject(true)

  private readonly router = inject(Router)
  private readonly marketplaceService = inject(AbstractMarketplaceService)
  readonly url = this.router.routerState.snapshot.root.queryParamMap.get('url')

  readonly loadVersion$ = new BehaviorSubject<string>('*')
  readonly pkg$ = this.loadVersion$.pipe(
    switchMap(version =>
      this.marketplaceService.getPackage$(this.pkgId, version, this.url),
    ),
    tap(data => {
      this.loading$.next(false)
      return data
    }),
  )

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly exver: Exver,
  ) {}

  open(id: string) {
    this.router.navigate([], { queryParams: { id } })
  }

  presentAlertVersions(
    pkg: MarketplacePkg,
    version: TemplateRef<TuiDialogContext>,
  ) {
    this.dialogs
      .open<string>(version, {
        label: 'Versions',
        size: 's',
        data: {
          value: pkg.version,
          items: [...new Set(Object.keys(pkg.otherVersions))].sort(
            (a, b) => -1 * (this.exver.compareExver(a, b) || 0),
          ),
        },
      })
      .pipe(filter(Boolean))
      .subscribe(version => this.loadVersion$.next(version))
  }
}
