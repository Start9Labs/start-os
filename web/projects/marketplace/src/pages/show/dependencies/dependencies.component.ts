import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { MarketplacePkgBase } from '../../../types'
import { MarketplaceDepItemComponent } from './dependency-item.component'

@Component({
  selector: 'marketplace-dependencies',
  template: `
    <div class="background-border shadow-color-light box-shadow-lg">
      <div class="dependencies-container">
        <h2 class="additional-detail-title">Dependencies</h2>
        <div class="dependencies-list">
          @for (dep of pkg.dependencyMetadata | keyvalue; track $index) {
            <marketplace-dep-item
              [dep]="dep"
              [pkg]="pkg"
              (click)="open.emit(dep.key)"
            />
          }
        </div>
      </div>
    </div>
  `,
  styles: [
    `
      .dependencies-container {
        background-color: rgb(39 39 42);
        border-radius: 0.75rem;
        padding: 1.75rem;

        @media (min-width: 1024px) {
          grid-column: span 5 / span 5;
        }
        @media (min-width: 1280px) {
          grid-column: span 4 / span 4;
        }
      }

      .dependencies-list {
        display: grid;
        grid-auto-rows: auto;
        gap: 0.75rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, MarketplaceDepItemComponent],
})
export class MarketplaceDependenciesComponent {
  @Input({ required: true })
  pkg!: MarketplacePkgBase

  @Output() open = new EventEmitter<string>()
}
