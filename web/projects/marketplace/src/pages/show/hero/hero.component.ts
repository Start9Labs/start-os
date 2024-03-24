import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { SharedPipesModule } from '@start9labs/shared'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-package-hero',
  template: `
    <div class="outer-container">
      <div class="inner-container box-shadow-lg">
        <!-- icon -->
        <img
          [src]="pkg.icon | trustUrl"
          class="box-shadow-lg"
          alt="{{ pkg.manifest.title }} Icon"
        />
        <!-- color background -->
        <div class="color-background">
          <img
            [src]="pkg.icon | trustUrl"
            alt="{{ pkg.manifest.title }} background image"
          />
        </div>
        <!-- background darkening overlay -->
        <div class="dark-overlay"></div>
        <div class="inner-container-title">
          <h2>
            {{ pkg.manifest.title }}
          </h2>
          <p>
            {{ pkg.manifest.description.short }}
          </p>
        </div>
        <!-- control buttons -->
        <ng-content></ng-content>
      </div>
    </div>
  `,
  styles: [
    `
      .outer-container {
        display: flex;
        justify-content: center;
        margin-top: 2.5rem;
        z-index: 0;

        @media (min-width: 768px) {
          margin-top: 0px;
        }
      }

      .inner-container {
        display: flex;
        flex-direction: column;
        width: 100%;
        height: 32vh;
        position: relative;
        border-radius: 1.5rem;
        padding: 4rem 2rem 0 2rem;

        @media (min-width: 376px) {
          height: 26vh;
        }
        @media (min-width: 768px) {
          min-height: 14rem;
        }

        img {
          width: 6rem;
          height: 6rem;
          pointer-events: none;
          border-radius: 9999px;
          object-fit: cover;
          position: absolute;
          top: -2.25rem;
          left: 1.75rem;
          z-index: 1;
        }

        .inner-container-title {
          margin: 1rem 0;
          color: rgb(250 250 250);
          mix-blend-mode: plus-lighter;
          z-index: 1;

          h2 {
            font-size: 1.5rem;
            line-height: 2rem;
            font-weight: 500;
            overflow: hidden;
            display: -webkit-box;
            -webkit-box-orient: vertical;
            -webkit-line-clamp: 1;
            margin-bottom: 0.25rem;
          }

          p {
            display: block;
            font-size: 1rem;
            line-height: 1.5rem;
            overflow: hidden;
            display: -webkit-box;
            -webkit-box-orient: vertical;
            -webkit-line-clamp: 2;
          }
        }

        .color-background {
          overflow: hidden;
          position: absolute;
          width: 100%;
          height: 100%;
          top: 0px;
          left: 0px;
          z-index: -50;
          border-radius: 1.5rem;
          background-color: rgb(39 39 42);

          img {
            position: absolute;
            object-fit: cover;
            pointer-events: none;
            width: 200%;
            height: 200%;
            max-width: 200%;
            filter: blur(100px) saturate(1.5);
          }
        }

        .dark-overlay {
          overflow: hidden;
          position: absolute;
          width: 100%;
          height: 100%;
          top: 0px;
          left: 0px;
          border-radius: 1.5rem;
          background-color: rgb(63 63 70);
          opacity: 0.7;
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, SharedPipesModule],
})
export class MarketplacePackageHeroComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg
}
