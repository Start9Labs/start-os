import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Input,
  inject,
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
        class="overflow-y-auto max-w-full md:max-w-[30rem]"
        [pkg]="pkg"
        (tuiActiveZoneChange)="toggle($event)"
      >
        <button
          tuiAutoFocus
          slot="close"
          size="xs"
          class="place-self-end"
          tuiIconButton
          type="button"
          appearance="icon"
          iconLeft="tuiIconClose"
          (click)="toggle(false)"
        ></button>
        <marketplace-controls
          slot="controls"
          class="flex justify-start gap-2"
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
