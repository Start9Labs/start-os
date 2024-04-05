import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'
import { ItemModule, MarketplacePkg } from '@start9labs/marketplace'
import { TuiSidebarModule } from '@taiga-ui/addon-mobile'
import {
  TuiActiveZoneModule,
  TuiAutoFocusModule,
  TuiDropdownPortalService,
} from '@taiga-ui/cdk'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { debounceTime, map } from 'rxjs'
import { ToLocalPipe } from '../pipes/to-local.pipe'
import { MarketplaceControlsComponent } from './controls.component'
import { MarketplacePreviewComponent } from '../modals/preview.component'
import { MarketplaceSidebarService } from '../services/sidebar.service'

@Component({
  selector: 'marketplace-tile',
  template: `
    <marketplace-item [pkg]="pkg" (click)="toggle(true)">
      <marketplace-preview
        *tuiSidebar="
          (id$ | async) === pkg.manifest.id;
          direction: 'right';
          autoWidth: true
        "
        [pkgId]="pkg.manifest.id"
        class="preview-wrapper"
      >
        <button
          tuiAutoFocus
          slot="close"
          size="xs"
          class="close-button"
          tuiIconButton
          type="button"
          appearance="icon"
          iconLeft="tuiIconClose"
          (click)="toggle(false)"
        ></button>
        <marketplace-controls
          slot="controls"
          class="controls-wrapper"
          [pkg]="pkg"
          [localPkg]="pkg.manifest.id | toLocal | async"
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
      provide: TuiDropdownPortalService,
      useExisting: MarketplaceSidebarService,
    },
  ],
  imports: [
    CommonModule,
    ItemModule,
    ToLocalPipe,
    TuiActiveZoneModule,
    TuiSidebarModule,
    TuiButtonModule,
    MarketplaceControlsComponent,
    MarketplacePreviewComponent,
    TuiAutoFocusModule,
  ],
})
export class MarketplaceTileComponent {
  private readonly router = inject(Router)
  readonly id$ = inject(ActivatedRoute).queryParamMap.pipe(
    map(map => map.get('id') || ''),
    debounceTime(100),
  )

  @Input({ required: true })
  pkg!: MarketplacePkg

  toggle(open: boolean) {
    this.router.navigate([], {
      queryParams: { id: open ? this.pkg.manifest.id : null },
    })
  }
}
