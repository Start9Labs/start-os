import { NgIf } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { StoreIconComponentModule } from '@start9labs/marketplace'
import { TuiIconModule, TuiTitleModule } from '@taiga-ui/experimental'
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
    <tui-icon
      *ngIf="registry.selected; else content"
      icon="tuiIconCheck"
      [style.color]="'var(--tui-positive)'"
    />
    <ng-template #content><ng-content></ng-content></ng-template>
  `,
  styles: [':host { border-radius: 0.25rem; width: stretch; }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [NgIf, StoreIconComponentModule, TuiIconModule, TuiTitleModule],
})
export class MarketplaceRegistryComponent {
  readonly marketplace = inject(ConfigService).marketplace

  @Input()
  registry!: { url: string; selected: boolean; name?: string }
}