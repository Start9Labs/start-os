import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { SharedPipesModule, TickerComponent } from '@start9labs/shared'

@Component({
  selector: 'marketplace-package-hero',
  template: `
    <div class="outer-container">
      <div class="inner-container box-shadow-lg">
        <!-- icon -->
        <img [src]="determineIcon()" alt="{{ pkg.title }} Icon" />
        <!-- color background -->
        <div class="color-background">
          <img [src]="determineIcon()" alt="{{ pkg.title }} background image" />
        </div>
        <!-- background darkening overlay -->
        <div class="dark-overlay"></div>
        <div class="inner-container-title">
          <h2 ticker>{{ pkg.title }}</h2>
          <h3>{{ pkg.version }}</h3>
          <p>{{ pkg.description.short }}</p>
        </div>
        <!-- control buttons -->
        <ng-content />
      </div>
    </div>
  `,
  styles: `
    .outer-container {
      display: flex;
      justify-content: center;
      margin-top: 2.5rem;

      @media (min-width: 768px) {
        margin-top: 0;
      }
    }

    .inner-container {
      display: flex;
      flex-direction: column;
      width: 100%;
      min-height: 32vh;
      position: relative;
      border-radius: 1.5rem;
      padding: 4rem 2rem 0 2rem;

      @media (min-width: 376px) {
        min-height: 20vh;
      }

      @media (min-width: 768px) {
        min-height: 11rem;
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
        max-width: 17rem;

        @media (min-width: 768px) {
          min-width: 100%;
        }

        h2 {
          font-size: 2rem;
          line-height: 3rem;
          font-weight: 400;
          display: inline-block;
          margin-left: -1px;
        }

        h3 {
          font-size: 1.1rem;
          font-weight: 400;
          margin-bottom: 1rem;
          pointer-events: none;
        }

        p {
          font-size: 1rem;
          line-height: 1.5rem;
          font-weight: 300;
          pointer-events: none;
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
        top: 0;
        left: 0;
        z-index: -50;
        border-radius: 1.5rem;

        img {
          position: absolute;
          object-fit: cover;
          pointer-events: none;
          width: 200%;
          height: 200%;
          max-width: 200%;
          filter: blur(100px);
        }
      }

      .dark-overlay {
        overflow: hidden;
        position: absolute;
        width: 100%;
        height: 100%;
        top: 0;
        left: 0;
        border-radius: 1.5rem;
        background-color: rgb(63 63 70);
        opacity: 0.7;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, SharedPipesModule, TickerComponent],
})
export class MarketplacePackageHeroComponent {
  @Input({ required: true })
  pkg!: {
    id: string
    title: string
    version: string
    description: { short: string }
    icon: string
  }

  determineIcon() {
    return this.pkg.icon
      ? this.pkg.icon
      : 'assets/img/service-icons/fallback.png'
  }
}
