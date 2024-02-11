import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import {
  TuiButtonModule,
  TuiDialogContext,
  TuiDialogService,
} from '@taiga-ui/core'
import { TuiCarouselModule } from '@taiga-ui/kit'
import { MarketplacePkg } from '../../../types'
import { PolymorpheusContent } from '@tinkoff/ng-polymorpheus'

@Component({
  selector: 'marketplace-package-screenshots',
  template: `
    <div
      *ngIf="pkg.screenshots"
      tuiCarouselButtons
      class="flex items-center content-center m-0 lg:-ml-14 lg:-mr-14 lg:min-h-80 lg:h-80 2xl:h-full"
    >
      <button
        tuiIconButton
        appearance="flat"
        icon="tuiIconChevronLeftLarge"
        title="Previous"
        type="button"
        (click)="carousel.prev()"
      ></button>
      <tui-carousel
        #carousel
        [itemsCount]="isMobile ? 1 : 2"
        [(index)]="index"
        class="overflow-y-hidden overflow-x-scroll carousel overflow-hidden"
      >
        <ng-container *ngFor="let item of pkg.screenshots; let i = index">
          <div
            *tuiItem
            draggable="false"
            [class.item_active]="i === index + 1"
            class="object-cover overflow-hidden rounded-lg md:rounded-xl border border-zinc-400/30 hover:cursor-pointer shadow-lg shadow-zinc-400/10"
          >
            <img
              #template
              alt="Service screenshot"
              src="assets/img/temp/{{ item }}"
              class="w-full h-full rounded-lg md:rounded-xl"
              (click)="presentModalImg(dialogTemplate)"
            />
            <ng-template #dialogTemplate let-observer>
              <img
                alt="Service screenshot"
                src="assets/img/temp/{{ item }}"
                class="rounded-none"
              />
            </ng-template>
          </div>
        </ng-container>
      </tui-carousel>
      <button
        tuiIconButton
        appearance="flat"
        type="button"
        icon="tuiIconChevronRightLarge"
        title="Next"
        (click)="carousel.next()"
      ></button>
    </div>
  `,
  styles: [],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiCarouselModule, TuiButtonModule],
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
