import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
  output,
} from '@angular/core'
import { MarketplacePkgBase } from '../../types'
import { CopyService } from '@start9labs/shared'
import { DatePipe } from '@angular/common'
import { MarketplaceItemComponent } from './item.component'

@Component({
  selector: 'marketplace-about',
  template: `
    <div class="background-border box-shadow-lg shadow-color-light">
      <div class="box-container">
        <div class="detail-container">
          <!-- version -->
          <marketplace-item
            [style.pointer-events]="'none'"
            [data]="pkg().version"
            label="Version"
            icon=""
          />
          <!-- release date -->
          @if (pkg().s9pk?.publishedAt; as published) {
            <marketplace-item
              [style.pointer-events]="'none'"
              [data]="(published | date: 'medium')!"
              label="Released"
              icon=""
            />
          }
          <!-- SDK version -->
          <marketplace-item
            [style.pointer-events]="'none'"
            [data]="pkg().sdkVersion || 'Unknown'"
            label="SDK version"
            icon=""
          />
          <!-- git hash -->
          @if (pkg().gitHash; as gitHash) {
            <marketplace-item
              (click)="copyService.copy(gitHash)"
              [data]="gitHash"
              label="Git Hash"
              icon="@tui.copy"
              class="item-copy"
            />
          } @else {
            <div class="item-padding">
              <label tuiTitle>
                <span tuiSubtitle>Git Hash</span>
                Unknown
              </label>
            </div>
          }
          <!-- license -->
          <marketplace-item
            (click)="static.emit('license')"
            [data]="pkg().license"
            label="License"
            icon="@tui.chevron-right"
            class="item-pointer"
          />
        </div>
      </div>
    </div>

    <div class="background-border box-shadow-lg shadow-color-light">
      <div class="box-container">
        <p [innerHTML]="pkg().description.long"></p>
      </div>
    </div>
  `,
  styles: `
    .box-container {
      background-color: rgb(39 39 42);
      border-radius: 0.75rem;
      padding: 1.25rem 1.75rem;

      p {
        font-size: 1rem;
        line-height: 1.5rem;
        margin-bottom: 0.75rem;
        pointer-events: none;
      }
    }

    .detail-container {
      display: grid;
      grid-auto-flow: row;
      grid-auto-columns: minmax(0, 1fr);

      & > * + * {
        border-top-width: 1px;
        border-bottom-width: 0;
        border-color: rgb(113 113 122);
      }
    }

    .item-pointer:hover {
      cursor: pointer;

      ::ng-deep label {
        cursor: pointer;
      }
    }

    .item-copy:hover {
      cursor: copy;

      ::ng-deep label {
        cursor: copy;
      }
    }

    .item-padding {
      padding: 0.75rem 0.25rem;
    }

    * {
      box-sizing: border-box;
      border-width: 0;
      border-style: solid;
      border-color: rgb(var(--tw-color-gray-200) / 1);
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [MarketplaceItemComponent, DatePipe],
})
export class MarketplaceAboutComponent {
  readonly copyService = inject(CopyService)

  readonly pkg = input.required<MarketplacePkgBase>()

  readonly static = output<'license'>()
}
