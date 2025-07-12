import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute, Router } from '@angular/router'
import { ItemModule, MarketplacePkg } from '@start9labs/marketplace'
import { TuiAutoFocus } from '@taiga-ui/cdk'
import { TuiButton, TuiDropdownService, TuiPopup } from '@taiga-ui/core'
import { TuiDrawer } from '@taiga-ui/kit'
import { debounceTime } from 'rxjs'
import { MarketplacePreviewComponent } from '../modals/preview.component'
import { MarketplaceSidebarService } from '../services/sidebar.service'

@Component({
  selector: 'marketplace-tile',
  template: `
    <marketplace-item [pkg]="pkg()" (click)="toggle(true)">
      <tui-drawer
        *tuiPopup="open()"
        [overlay]="true"
        (click.self)="toggle(false)"
      >
        <marketplace-preview [pkgId]="pkg().id">
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
        </marketplace-preview>
      </tui-drawer>
    </marketplace-item>
  `,
  styles: `
    @keyframes animateIn {
      from {
        opacity: 0;
        transform: scale(0.6) translateY(-20px);
      }
    }

    :host {
      cursor: pointer;
      animation: animateIn 400ms calc(var(--animation-order) * 200ms) both;
    }

    tui-drawer {
      top: 0;
      width: 28rem;
      border-radius: 0;
    }

    button {
      place-self: end;
      margin-bottom: 0;

      @media (min-width: 768px) {
        margin-bottom: 2rem;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
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
    TuiButton,
    TuiPopup,
    TuiDrawer,
    MarketplacePreviewComponent,
  ],
})
export class MarketplaceTileComponent {
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
