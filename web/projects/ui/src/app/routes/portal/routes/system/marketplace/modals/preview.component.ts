import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  TemplateRef,
} from '@angular/core'
import {
  AboutModule,
  AbstractMarketplaceService,
  AdditionalModule,
  MarketplaceAdditionalItemComponent,
  MarketplaceDependenciesComponent,
  MarketplacePackageHeroComponent,
  MarketplacePkg,
  ReleaseNotesModule,
  StoreIdentity,
} from '@start9labs/marketplace'
import { displayEmver, Emver, SharedPipesModule } from '@start9labs/shared'
import { TuiButtonModule, TuiIconModule } from '@taiga-ui/experimental'
import { BehaviorSubject, filter, switchMap, tap } from 'rxjs'
import {
  TuiDialogContext,
  TuiDialogService,
  TuiLoaderModule,
} from '@taiga-ui/core'
import {
  TuiRadioListModule,
  TuiStringifyContentPipeModule,
} from '@taiga-ui/kit'
import { FormsModule } from '@angular/forms'
import { Router } from '@angular/router'

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
            @if (!(pkg.manifest.dependencies | empty)) {
              <marketplace-dependencies
                [pkg]="pkg"
                (open)="open($event)"
              ></marketplace-dependencies>
            }
            <release-notes [pkg]="pkg" />
            <marketplace-additional [pkg]="pkg">
              <marketplace-additional-item
                (click)="presentAlertVersions(pkg, version)"
                data="Click to view all versions"
                label="All versions"
                icon="tuiIconChevronRightLarge"
                class="versions"
              ></marketplace-additional-item>
              <ng-template
                #version
                let-data="data"
                let-completeWith="completeWith"
              >
                <tui-radio-list
                  size="l"
                  [items]="data.items"
                  [itemContent]="displayEmver | tuiStringifyContent"
                  [(ngModel)]="data.value"
                ></tui-radio-list>
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
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    MarketplacePackageHeroComponent,
    TuiButtonModule,
    MarketplaceDependenciesComponent,
    ReleaseNotesModule,
    AdditionalModule,
    AboutModule,
    SharedPipesModule,
    FormsModule,
    TuiStringifyContentPipeModule,
    MarketplaceAdditionalItemComponent,
    TuiRadioListModule,
    TuiLoaderModule,
    TuiIconModule,
  ],
})
export class MarketplacePreviewComponent {
  @Input({ required: true })
  pkgId!: string

  readonly loading$ = new BehaviorSubject(true)

  readonly displayEmver = displayEmver
  private readonly router = inject(Router)
  private readonly marketplaceService = inject(AbstractMarketplaceService)
  readonly url =
    this.router.routerState.snapshot.root.queryParamMap.get('url') || undefined

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
    private readonly emver: Emver,
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
          value: pkg.manifest.version,
          items: [...new Set(pkg.versions)].sort(
            (a, b) => -1 * (this.emver.compare(a, b) || 0),
          ),
        },
      })
      .pipe(filter(Boolean))
      .subscribe(version => this.loadVersion$.next(version))
  }
}
