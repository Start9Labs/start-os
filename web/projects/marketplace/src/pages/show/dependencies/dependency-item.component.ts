import { CommonModule, KeyValue } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterModule } from '@angular/router'
import { ExverPipesModule } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiAvatar, TuiLineClamp } from '@taiga-ui/kit'
import { MarketplacePkgBase } from '../../../types'

@Component({
  selector: 'marketplace-dep-item',
  template: `
    <div class="outer-container">
      <tui-avatar class="dep-img" size="l" [src]="getImage(dep.key)" />
      <div>
        <tui-line-clamp [linesLimit]="2" [content]="titleContent" />
        <ng-template #titleContent>
          <div class="title">
            <span>
              {{ getTitle(dep.key) }}
            </span>
            <p>
              <ng-container [ngSwitch]="dep.value.optional">
                <span *ngSwitchCase="true">(optional)</span>
                <span *ngSwitchCase="false">(required)</span>
              </ng-container>
            </p>
          </div>
        </ng-template>
        <tui-line-clamp
          [linesLimit]="2"
          [content]="descContent"
          class="description"
        />
        <ng-template #descContent>
          {{ dep.value.description }}
        </ng-template>
      </div>
    </div>
  `,
  styles: [
    `
      .outer-container {
        display: flex;
        align-items: center;
        gap: 1.5rem;
        padding: 1rem 1.25rem;
        background-color: rgb(63 63 70 / 0.4);
        border-radius: 0.75rem;
        filter: drop-shadow(0 10px 8px rgb(0 0 0 / 0.04))
          drop-shadow(0 4px 3px rgb(0 0 0 / 0.1));

        &:hover {
          background-color: rgb(63 63 70 / 0.7);
          cursor: pointer;
        }
      }

      .title {
        display: flex;
        gap: 0;
        flex-wrap: wrap;

        @media (min-width: 640px) {
          gap: 0.25rem;
        }

        p {
          margin: 0;
        }

        span {
          font-size: 1rem;
          line-height: 1.5rem;
          font-weight: 500;
          color: rgb(250 250 250 / 0.9);
        }
      }

      .description {
        font-size: 0.875rem;
        line-height: 1.25rem;
        color: rgb(250 250 250 / 0.7);
      }

      ::ng-deep .dep-img {
        filter: drop-shadow(0 10px 8px rgb(0 0 0 / 0.04))
          drop-shadow(0 4px 3px rgb(0 0 0 / 0.1));
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    RouterModule,
    TuiAvatar,
    ExverPipesModule,
    TuiLineClamp,
  ],
})
export class MarketplaceDepItemComponent {
  @Input({ required: true })
  pkg!: MarketplacePkgBase

  @Input({ required: true })
  dep!: KeyValue<string, T.DependencyMetadata>

  getImage(key: string) {
    const icon = this.pkg.dependencyMetadata[key]?.icon
    return icon ? icon : 'assets/img/service-icons/fallback.png'
  }

  getTitle(key: string): string {
    return this.pkg.dependencyMetadata[key]?.title || key
  }
}
