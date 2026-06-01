import { ChangeDetectionStrategy, Component, input, Input } from '@angular/core'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'

import { StoreIconDirective } from './store-icon.directive'

@Component({
  selector: '[registry]',
  template: `
    <span tuiAvatar><img [storeIcon]="registry().url" /></span>
    <div tuiTitle>
      {{ registry().name }}
      <div tuiSubtitle>{{ registry().url }}</div>
    </div>
    @if (registry().selected) {
      <tui-icon icon="@tui.check" class="g-positive" />
    } @else {
      <ng-content />
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [StoreIconDirective, TuiIcon, TuiTitle, TuiAvatar],
})
export class MarketplaceRegistryComponent {
  registry = input.required<{ url: string; selected: boolean; name: string }>()
}
