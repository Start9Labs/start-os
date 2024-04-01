import { CommonModule, KeyValue } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { EmverPipesModule } from '@start9labs/shared'
import { Dependency, MarketplacePkg } from '../../../types'
import { RouterModule } from '@angular/router'
import { TuiAvatarModule } from '@taiga-ui/experimental'

@Component({
  selector: 'marketplace-dep-item',
  template: `
    <div class="outer-container">
      <div class="inner-container">
        <tui-avatar class="dep-img" [src]="getImage(dep.key)"></tui-avatar>
        <div class="wrapper-margin">
          <div class="inner-container-title">
            <span>
              {{ getTitle(dep.key) }}
            </span>
            <p>
              <ng-container [ngSwitch]="dep.value.optional">
                <span *ngSwitchCase="true">(required)</span>
                <span *ngSwitchCase="false">(optional)</span>
              </ng-container>
            </p>
          </div>
          <span class="inner-container-version">
            <!-- {{ dep.value.version | displayEmver }} -->
          </span>
          <span class="inner-container-description">
            {{ dep.value.description }}
          </span>
        </div>
      </div>
    </div>
  `,
  styles: [
    `
      .outer-container {
        background-color: rgb(63 63 70 / 0.4);
        border-radius: 0.75rem;
        padding: 0.75rem 1.25rem;
        gap: 0.5rem;
        filter: drop-shadow(0 10px 8px rgb(0 0 0 / 0.04))
          drop-shadow(0 4px 3px rgb(0 0 0 / 0.1));

        &:hover {
          background-color: rgb(63 63 70 / 0.7);
          cursor: pointer;
        }
      }

      .inner-container {
        display: flex;
        align-items: center;
        gap: 1.5rem;
      }

      .inner-container-title {
        margin-bottom: 0.25rem;
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 0.25rem;

        span {
          display: block;
          font-size: 1rem;
          line-height: 1.5rem;
          font-weight: 500;
          color: rgb(250 250 250 / 0.9);
          overflow: hidden;
          display: -webkit-box;
          -webkit-box-orient: vertical;
          -webkit-line-clamp: 1;
        }
      }

      .inner-container-version {
        font-size: 0.875rem;
        line-height: 1.25rem;
        color: rgb(250 250 250 / 0.7);
        overflow: hidden;
        display: -webkit-box;
        -webkit-box-orient: vertical;
        -webkit-line-clamp: 1;
      }

      .inner-container-description {
        font-size: 0.875rem;
        line-height: 1.25rem;
        color: rgb(250 250 250 / 0.7);
        height: 2.75rem;
        overflow: hidden;
        display: -webkit-box;
        -webkit-box-orient: vertical;
        -webkit-line-clamp: 2;
      }

      ::ng-deep .dep-img {
        width: 4rem;
        pointer-events: none;
        border-radius: 100%;
        object-fit: cover;
        filter: drop-shadow(0 10px 8px rgb(0 0 0 / 0.04))
          drop-shadow(0 4px 3px rgb(0 0 0 / 0.1));
      }

      .wrapper-margin {
        margin-top: 0.75rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, RouterModule, TuiAvatarModule, EmverPipesModule],
})
export class MarketplaceDepItemComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  @Input({ required: true })
  dep!: KeyValue<string, Dependency>

  getImage(key: string): string {
    const icon = this.pkg.dependencyMetadata[key]?.icon
    // @TODO fix when registry api is updated to include mimetype in icon url
    return icon ? `data:image/png;base64,${icon}` : key.substring(0, 2)
  }

  getTitle(key: string): string {
    return this.pkg.dependencyMetadata[key]?.title || key
  }
}
