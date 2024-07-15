import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { StoreIconComponentModule } from '@start9labs/marketplace'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { ConfigService } from 'src/app/services/config.service'

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
  readonly marketplace = inject(ConfigService).marketplace

  @Input()
  registry!: { url: string; selected: boolean; name?: string }
}
