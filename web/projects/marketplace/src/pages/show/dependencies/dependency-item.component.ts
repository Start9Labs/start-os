import { CommonModule, KeyValue } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { EmverPipesModule } from '@start9labs/shared'
import { Dependency, MarketplacePkg } from '../../../types'
import { RouterModule } from '@angular/router'
import { TuiAvatarModule, TuiLineClampModule } from '@taiga-ui/kit'

@Component({
  selector: 'marketplace-dep-item',
  template: `
    <div class="outer-container">
      <tui-avatar
        class="dep-img"
        [rounded]="true"
        [size]="'l'"
        [avatarUrl]="getImage(dep.key)"
      ></tui-avatar>
      <div>
        <tui-line-clamp
          [linesLimit]="2"
          [content]="titleContent"
        ></tui-line-clamp>
        <ng-template #titleContent>
          <div class="title">
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
        </ng-template>
        <tui-line-clamp
          [linesLimit]="2"
          [content]="descContent"
          class="description"
        ></tui-line-clamp>
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
        gap: 0.25rem;

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
    TuiAvatarModule,
    EmverPipesModule,
    TuiLineClampModule,
  ],
})
export class MarketplaceDepItemComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  @Input({ required: true })
  dep!: KeyValue<string, Dependency>

  getImage(key: string): string {
    const icon = this.pkg.dependencyMetadata[key]?.icon
    // @TODO fix when registry api is updated to include mimetype in icon url
    // return icon ? `data:image/png;base64,${icon}` : key.substring(0, 2)
    return icon ? icon : key.substring(0, 2)
  }

  getTitle(key: string): string {
    return this.pkg.dependencyMetadata[key]?.title || key
  }
}
