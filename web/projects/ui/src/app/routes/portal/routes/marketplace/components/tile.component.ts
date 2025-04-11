import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { toObservable, toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute, Router } from '@angular/router'
import { ItemModule, MarketplacePkg } from '@start9labs/marketplace'
import { Exver } from '@start9labs/shared'
import { TuiSidebar } from '@taiga-ui/addon-mobile'
import { TuiAutoFocus, TuiClickOutside } from '@taiga-ui/cdk'
import { TuiButton, TuiDropdownService } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import {
  debounceTime,
  filter,
  map,
  Observable,
  shareReplay,
  switchMap,
} from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import { MarketplacePreviewComponent } from '../modals/preview.component'
import { MarketplaceSidebarService } from '../services/sidebar.service'
import { MarketplaceControlsComponent } from './controls.component'

@Component({
  selector: 'marketplace-tile',
  template: `
    <marketplace-item [pkg]="pkg()" (click)="toggle(true)">
      <marketplace-preview
        *tuiSidebar="!!open(); direction: 'right'; autoWidth: true"
        [pkgId]="pkg().id"
        class="preview-wrapper"
        (tuiClickOutside)="toggle(false)"
      >
        <button
          tuiAutoFocus
          slot="close"
          size="xs"
          class="close-button"
          tuiIconButton
          type="button"
          appearance="icon"
          iconStart="@tui.x"
          [tuiAppearanceFocus]="false"
          (click)="toggle(false)"
        ></button>
        <marketplace-controls
          slot="controls"
          class="controls-wrapper"
          [pkg]="pkg()"
          [localPkg]="local$ | async"
          [localFlavor]="!!(flavor$ | async)"
        />
      </marketplace-preview>
    </marketplace-item>
  `,
  styles: [
    `
      :host {
        animation: animateIn 400ms calc(var(--animation-order) * 200ms) both;
      }

      @keyframes animateIn {
        from {
          opacity: 0;
          transform: scale(0.6) translateY(-20px);
        }

        to {
          opacity: 1;
        }
      }

      .preview-wrapper {
        overflow-y: auto;
        height: 100%;
        max-width: 100%;

        @media (min-width: 768px) {
          max-width: 30rem;
        }
      }

      .close-button {
        place-self: end;
        margin-bottom: 0;

        @media (min-width: 768px) {
          margin-bottom: 2rem;
        }
      }

      .controls-wrapper {
        display: flex;
        justify-content: flex-start;
        gap: 0.5rem;
        height: 4.5rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  providers: [
    {
      provide: TuiDropdownService,
      useExisting: MarketplaceSidebarService,
    },
  ],
  imports: [
    CommonModule,
    ItemModule,
    TuiAutoFocus,
    TuiClickOutside,
    TuiSidebar,
    TuiButton,
    MarketplaceControlsComponent,
    MarketplacePreviewComponent,
  ],
})
export class MarketplaceTileComponent {
  private readonly exver = inject(Exver)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly router = inject(Router)
  private readonly params = toSignal(
    inject(ActivatedRoute).queryParamMap.pipe(debounceTime(100)),
  )

  readonly pkg = input.required<MarketplacePkg>()
  readonly open = computed(
    () =>
      this.params()?.get('id') === this.pkg()?.id &&
      this.params()?.get('flavor') === this.pkg()?.flavor,
  )

  readonly local$: Observable<PackageDataEntry | null> = toObservable(
    this.pkg,
  ).pipe(
    switchMap(({ id, flavor }) =>
      this.patch.watch$('packageData', id).pipe(
        filter(Boolean),
        map(pkg =>
          this.exver.getFlavor(getManifest(pkg).version) === flavor
            ? pkg
            : null,
        ),
      ),
    ),
    shareReplay({ bufferSize: 1, refCount: true }),
  )

  readonly flavor$ = this.local$.pipe(map(pkg => !pkg))

  toggle(open: boolean) {
    this.router.navigate([], {
      queryParams: {
        id: open ? this.pkg().id : null,
        flavor: open ? this.pkg().flavor : null,
      },
      queryParamsHandling: 'merge',
    })
  }
}
