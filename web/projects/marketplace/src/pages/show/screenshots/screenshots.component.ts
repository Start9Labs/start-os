import { TuiCarousel } from '@taiga-ui/kit'
import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import { TuiDialogContext, TuiDialogService, TuiButton } from '@taiga-ui/core'
import { MarketplacePkg } from '../../../types'
import { PolymorpheusContent } from '@taiga-ui/polymorpheus'

@Component({
  selector: 'marketplace-package-screenshots',
  template: `
    <!--@TODO future release-->
    <div
      *ngIf="$any(pkg).screenshots as screenshots"
      tuiCarouselButtons
      class="outer-container"
    >
      <button
        tuiIconButton
        appearance="flat-grayscale"
        iconStart="@tui.chevron-left"
        title="Previous"
        type="button"
        (click)="carousel.prev()"
      ></button>
      <tui-carousel
        #carousel
        [itemsCount]="isMobile ? 1 : 2"
        [(index)]="index"
        class="carousel"
      >
        <ng-container *ngFor="let item of screenshots; let i = index">
          <div
            *tuiItem
            draggable="false"
            [class.item_active]="i === index + 1"
            class="screenshot-item"
          >
            <img
              #template
              alt="Service screenshot"
              src="assets/img/temp/{{ item }}"
              class="screenshot-item-img"
              (click)="presentModalImg(dialogTemplate)"
            />
            <ng-template #dialogTemplate let-observer>
              <img
                alt="Service screenshot"
                src="assets/img/temp/{{ item }}"
                class="screenshot-item-img-enlarged"
              />
            </ng-template>
          </div>
        </ng-container>
      </tui-carousel>
      <button
        tuiIconButton
        appearance="flat-grayscale"
        type="button"
        iconStart="@tui.chevron-right"
        title="Next"
        (click)="carousel.next()"
      ></button>
    </div>
  `,
  styles: [
    `
      .outer-container {
        display: flex;
        align-items: center;
        align-content: center;
        margin: 0px;

        @media (min-width: 1024px) {
          margin-left: -3.5rem;
          margin-right: -3.5rem;
          min-height: 20rem;
          height: 20rem;
        }
        @media (min-width: 1536px) {
          height: 100%;
        }
      }

      .carousel {
        overflow-y: hidden;
        overflow-x: scroll;
        overflow: hidden;
      }

      .screenshot-item {
        object-fit: cover;
        overflow: hidden;
        border-radius: 0.5rem;
        border-width: 1px;
        border-color: rgb(161 161 170 / 0.3);

        &:hover {
          cursor: pointer;
        }

        @media (min-width: 768px) {
          border-radius: 0.75rem;
        }

        .screenshot-item-img {
          width: 100%;
          height: 100%;
          border-radius: 0.5rem;

          @media (min-width: 768px) {
            border-radius: 0.75rem;
          }

          .screenshot-item-img-enlarged {
            border-radius: 0px;
          }
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiCarousel, TuiButton],
})
export class MarketplacePackageScreenshotComponent {
  private readonly dialogs = inject(TuiDialogService)

  @Input({ required: true })
  pkg!: MarketplacePkg

  index = 0

  isMobile = inject(TUI_IS_MOBILE)

  presentModalImg(content: PolymorpheusContent<TuiDialogContext>) {
    this.dialogs
      .open(content, {
        size: 'l',
      })
      .subscribe()
  }
}
