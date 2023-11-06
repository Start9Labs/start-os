import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { SharedPipesModule } from '@start9labs/shared'
import { MarketplacePkg } from '../../../types'
import { MimeTypePipeModule } from '../../../pipes/mime-type.pipe'

@Component({
  selector: 'marketplace-package-hero',
  template: `
    <div class="flex justify-center mt-10 md:mt-0 z-0">
      <div
        class="flex flex-col w-full h-[32vh] xs:h-[25vh] md:min-h-[14rem] relative rounded-3xl pt-16 pb-10 px-8 shadow-lg"
      >
        <!-- icon -->
        <img
          [src]="pkg | mimeType | trustUrl"
          class="w-24 h-24 pointer-events-none rounded-full object-cover shadow-lg absolute -top-9 left-7 z-10"
          alt="{{ pkg.manifest.title }} Icon"
        />
        <!-- color background -->
        <div
          class="overflow-hidden absolute w-full h-full top-0 left-0 rounded-3xl bg-zinc-800"
        >
          <img
            [src]="pkg | mimeType | trustUrl"
            class="absolute object-cover pointer-events-none w-[200%] h-[200%] max-w-[200%] blur-[100px] saturate-150 rounded-full"
            alt="{{ pkg.manifest.title }} background image"
          />
        </div>
        <div
          class="overflow-hidden absolute w-full h-full top-0 left-0 rounded-3xl bg-zinc-700 opacity-70"
        ></div>
        <div class="my-3 text-zinc-50 mix-blend-plus-lighter">
          <h2 class="text-2xl font-medium line-clamp-1 mb-1">
            {{ pkg.manifest.title }}
          </h2>
          <p class="block text-base line-clamp-2">
            {{ pkg.manifest.description.short }}
          </p>
        </div>
        <!-- control buttons -->
        <ng-content></ng-content>
      </div>
    </div>
  `,
  styles: [],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, SharedPipesModule, MimeTypePipeModule],
})
export class MarketplacePackageHeroComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg
}
