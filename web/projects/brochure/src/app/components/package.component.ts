import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { rxResource, toSignal } from '@angular/core/rxjs-interop'
import {
  MarketplaceAboutComponent,
  MarketplaceDependenciesComponent,
  MarketplaceFlavorsComponent,
  MarketplaceLinksComponent,
  MarketplacePkg,
  MarketplaceReleaseNotesComponent,
} from '@start9labs/marketplace'
import { EmptyPipe, i18nPipe, MarkdownComponent } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import {
  TuiDialogContext,
  TuiLoader,
  TuiNotification,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiAvatar, TuiFade } from '@taiga-ui/kit'
import { tuiCardOptionsProvider, TuiHeader } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { filter, map } from 'rxjs/operators'
import { MarketplaceService } from 'src/app/services/marketplace.service'

@Component({
  template: `
    @if (pkg.isLoading()) {
      <tui-loader
        size="xl"
        [style.min-height.rem]="20"
        [textContent]="'Loading' | i18n"
      />
    } @else if (pkg.error()) {
      <header tuiHeader [id]="context.id">Unknown package</header>
      <div tuiNotification appearance="negative">
        <div tuiTitle>
          Package not found
          <div tuiSubtitle>The URL you entered is invalid</div>
        </div>
      </div>
    } @else if (pkg.value(); as pkg) {
      <header tuiHeader [id]="context.id">
        <span tuiAvatar size="l" [round]="false">
          <img
            alt=""
            [src]="pkg.icon || 'assets/img/service-icons/fallback.png'"
          />
        </span>
        <hgroup tuiTitle>
          <h2 tuiFade>{{ pkg.title }}</h2>
          <p tuiSubtitle>{{ pkg.description.short }}</p>
        </hgroup>
      </header>
      <marketplace-about [pkg]="pkg" (static)="onStatic(pkg)" />
      <marketplace-release-notes [pkg]="pkg" />
      @if (flavors(); as flavors) {
        <marketplace-flavors [pkgs]="flavors" />
      }
      @if (!(pkg.dependencyMetadata | empty)) {
        <marketplace-dependencies [pkg]="pkg" />
      }
      <marketplace-links [pkg]="pkg" />
    }
  `,
  styles: `
    :host {
      display: grid;
      gap: 1rem;

      ::ng-deep [tuiCardLarge] {
        background: var(--tui-background-neutral-1);
      }
    }
  `,
  providers: [tuiCardOptionsProvider({ space: 'compact' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiHeader,
    TuiAvatar,
    TuiTitle,
    TuiFade,
    TuiLoader,
    TuiNotification,
    i18nPipe,
    EmptyPipe,
    MarketplaceAboutComponent,
    MarketplaceReleaseNotesComponent,
    MarketplaceFlavorsComponent,
    MarketplaceDependenciesComponent,
    MarketplaceLinksComponent,
  ],
})
export class PackageComponent {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly service = inject(MarketplaceService)

  protected readonly context =
    injectContext<TuiDialogContext<void, { id: string; flavor: string }>>()

  protected readonly pkg = rxResource({
    stream: () =>
      this.service.getPackage$(
        this.context.data.id,
        this.context.data.flavor || null,
      ),
  })

  protected readonly flavors = toSignal(
    this.service.getRegistry$().pipe(
      map(({ packages }) =>
        packages.filter(
          ({ id, flavor }) =>
            id === this.context.data.id && flavor !== this.context.data.flavor,
        ),
      ),
      filter(({ length }) => !!length),
    ),
  )

  onStatic(pkg: MarketplacePkg) {
    this.dialogs
      .open(new PolymorpheusComponent(MarkdownComponent), {
        label: 'License',
        size: 'l',
        data: this.service.getStatic$(pkg, 'LICENSE.md'),
      })
      .subscribe()
  }
}
