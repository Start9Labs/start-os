import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'
import { TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { MarketplacePkg } from '../../../types'

@Component({
  standalone: true,
  selector: 'marketplace-flavors',
  template: `
    <h2>Alternative Implementations</h2>
    @for (pkg of pkgs; track $index) {
      <a
        tuiCell
        [routerLink]="['/marketplace', pkg.id]"
        [queryParams]="{ flavor: pkg.flavor }"
      >
        <img alt="" style="border-radius: 100%" [src]="pkg.icon | trustUrl" />
        <span tuiTitle>
          {{ pkg.title }}
          <span tuiSubtitle>{{ pkg.version }}</span>
        </span>
      </a>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [RouterLink, TuiCell, TuiTitle, SharedPipesModule],
})
export class FlavorsComponent {
  @Input()
  pkgs!: MarketplacePkg[]
}
