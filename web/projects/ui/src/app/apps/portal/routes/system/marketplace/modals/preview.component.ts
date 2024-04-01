import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  inject,
  Input,
  Output,
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
} from '@start9labs/marketplace'
import { displayEmver, Emver, SharedPipesModule } from '@start9labs/shared'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { filter, map } from 'rxjs'
import { TuiDialogContext, TuiDialogService } from '@taiga-ui/core'
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
      <marketplace-package-hero [pkg]="pkg">
        <ng-content select="[slot=controls]" />
      </marketplace-package-hero>
      @if (url$ | async; as url) {
        <a
          [href]="url + '/marketplace/' + pkg.manifest.id"
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
            (click)="presentAlertVersions(version)"
            data="Click to view all versions"
            label="All versions"
            icon="tuiIconChevronRightLarge"
            class="item-pointer"
          ></marketplace-additional-item>
          <ng-template #version let-data="data" let-completeWith="completeWith">
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
                (click)="completeWith(data.value)"
              >
                Ok
              </button>
            </footer>
          </ng-template>
        </marketplace-additional>
      </div>
    </div>
  `,
  styles: [
    `
      :host {
        pointer-events: auto;
      }

      .outer-container {
        display: grid;
        justify-content: center;
        gap: 2rem;
        padding: 1.75rem;
      }

      .inner-container {
        display: grid;
        grid-template-columns: repeat(1, minmax(0, 1fr));
        column-gap: 2rem;
      }

      .additional-wrapper {
        margin-top: 1.5rem;
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
  ],
})
export class MarketplacePreviewComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  @Output()
  version = new EventEmitter<string>()

  readonly displayEmver = displayEmver
  private readonly router = inject(Router)
  readonly url$ = inject(AbstractMarketplaceService)
    .getSelectedHost$()
    .pipe(map(({ url }) => url))

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly emver: Emver,
  ) {}

  open(id: string) {
    this.router.navigate([], { queryParams: { id } })
  }

  presentAlertVersions(version: TemplateRef<TuiDialogContext>) {
    this.dialogs
      .open<string>(version, {
        label: 'Versions',
        size: 's',
        data: {
          value: this.pkg.manifest.version,
          items: [...new Set(this.pkg.versions)].sort(
            (a, b) => -1 * (this.emver.compare(a, b) || 0),
          ),
        },
      })
      .pipe(filter(Boolean))
      .subscribe(version => this.version.emit(version))
  }
}
