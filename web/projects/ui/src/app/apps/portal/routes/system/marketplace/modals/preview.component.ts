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
import { TuiButtonModule } from '@taiga-ui/experimental'
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
        @if (loading) {
          <tui-loader class="loading-dots" textContent="Loading" />
        } @else {
          <marketplace-package-hero [pkg]="pkg">
            <ng-content select="[slot=controls]" />
          </marketplace-package-hero>
          @if (hostInfo$ | async; as info) {
            <a
              [href]="constructDetailLink(info, pkg.manifest.id)"
              tuiButton
              appearance="tertiary-solid"
              iconRight="tuiIconExternalLink"
              target="_blank"
            >
              View more details
            </a>
          }
          <div class="inner-container">
            <marketplace-about [pkg]="pkg" />
            @if (!(pkg.manifest.dependencies | empty)) {
              <marketplace-dependencies
                [pkg]="pkg"
                (open)="open($event)"
              ></marketplace-dependencies>
            }
            <release-notes [pkg]="pkg" />
            <marketplace-additional class="additional-wrapper" [pkg]="pkg">
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
                    (click)="loading = true; completeWith(data.value)"
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
        display: grid;
        gap: 2rem;
        padding: 1.75rem;
        min-width: 30rem;
      }

      .inner-container {
        display: grid;
        grid-template-columns: repeat(1, minmax(0, 1fr));
        column-gap: 2rem;
      }

      .additional-wrapper {
        margin-top: 1.5rem;
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
  ],
})
export class MarketplacePreviewComponent {
  @Input({ required: true })
  pkgId!: string

  loading = true

  readonly displayEmver = displayEmver
  private readonly router = inject(Router)
  private readonly marketplaceService = inject(AbstractMarketplaceService)
  readonly url =
    this.router.routerState.snapshot.root.queryParamMap.get('url') || undefined

  readonly loadVersion$ = new BehaviorSubject<string>('*')
  readonly hostInfo$ = this.marketplaceService.getSelectedHost$()
  readonly pkg$ = this.loadVersion$.pipe(
    switchMap(version =>
      this.marketplaceService.getPackage$(this.pkgId, version, this.url),
    ),
    tap(data => {
      this.loading = false
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

  constructDetailLink(info: StoreIdentity, id: string) {
    const domain = new URL(info.url).hostname
    return `https://marketplace.start9.com/${id}?api=${domain}&name=${info.name}`
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
