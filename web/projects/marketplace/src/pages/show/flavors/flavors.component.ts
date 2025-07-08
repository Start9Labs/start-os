import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'
import { TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-flavors',
  template: `
    <div class="background-border box-shadow-lg shadow-color-light">
      <div class="box-container">
        <h2 class="additional-detail-title">Alternative Implementations</h2>
        @for (pkg of pkgs; track $index) {
          <a
            tuiCell
            [routerLink]="[]"
            [queryParams]="{ id: pkg.id, flavor: pkg.flavor }"
            queryParamsHandling="merge"
          >
            <tui-avatar [src]="pkg.icon | trustUrl" />
            <span tuiTitle>
              {{ pkg.title }}
              <span tuiSubtitle>{{ pkg.version }}</span>
            </span>
          </a>
        }
      </div>
    </div>
  `,
  styles: `
    .box-container {
      background-color: rgb(39 39 42);
      border-radius: 0.75rem;
      padding: 1.75rem;
    }

    [tuiCell] {
      border-radius: 0.5rem;
      margin: 0 -1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [RouterLink, TuiCell, TuiTitle, SharedPipesModule, TuiAvatar],
})
export class FlavorsComponent {
  @Input()
  pkgs!: MarketplacePkg[]
}
