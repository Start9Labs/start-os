import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { StoreIconComponentModule } from './store-icon/store-icon.component.module'
import { MarketplaceConfig } from '@start9labs/shared'

@Component({
  standalone: true,
  selector: '[registry]',
  template: `
    <store-icon [url]="registry.url" [marketplace]="marketplace" size="40px" />
    <div tuiTitle>
      {{ registry.name }}
      <div tuiSubtitle>{{ registry.url }}</div>
    </div>
    @if (registry.selected) {
      <tui-icon icon="@tui.check" [style.color]="'var(--tui-text-positive)'" />
    } @else {
      <ng-content />
    }
  `,
  styles: [':host { border-radius: 0.25rem; width: stretch; }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [StoreIconComponentModule, TuiIcon, TuiTitle],
})
export class MarketplaceRegistryComponent {
  @Input()
  marketplace!: MarketplaceConfig

  @Input()
  registry!: { url: string; selected: boolean; name?: string }
}
