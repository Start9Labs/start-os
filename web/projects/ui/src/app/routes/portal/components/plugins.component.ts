import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import { TuiAnimated } from '@taiga-ui/cdk'
import { TuiButton, TuiPopup } from '@taiga-ui/core'
import { ResizerComponent } from 'src/app/routes/portal/components/resizer.component'
import { PluginsService } from 'src/app/services/plugins.service'

@Component({
  selector: 'app-plugins',
  template: `
    <button
      tuiIconButton
      iconStart="@tui.bot-message-square"
      [appearance]="service.enabled() ? 'positive' : 'icon'"
      (click)="service.enabled.set(!service.enabled())"
    >
      AI assistant
    </button>
    <aside
      *tuiPopup="service.enabled()"
      tuiAnimated
      [class._mobile]="mobile"
      [style.--plugins]="service.size() / 100"
      (click.self)="onClick($any($event).layerX)"
    >
      Plugin placeholder
    </aside>
    <input
      *tuiPopup="service.enabled()"
      appResizer
      type="range"
      step="0.1"
      [(ngModel)]="service.size"
    />
  `,
  styles: `
    :host {
      float: inline-end;
      margin-inline-end: 0.5rem;
    }

    [tuiIconButton] {
      background: transparent;
    }

    aside {
      position: fixed;
      inset: 0 0 0 calc(320px + (100% - 640px) * var(--plugins));
      display: flex;
      place-content: center;
      place-items: center;
      margin: var(--bumper) var(--bumper) var(--bumper) 0;
      background: color-mix(in hsl, var(--start9-base-2) 75%, transparent);
      background-image: linear-gradient(
        transparent,
        var(--tui-background-base)
      );
      backdrop-filter: blur(1rem);
      border-radius: var(--bumper);

      --tui-from: translateX(100%);

      &._mobile {
        inset-inline-start: 20%;
        box-shadow:
          inset 0 1px rgba(255, 255, 255, 0.25),
          0 0 0 100vh rgb(0 0 0 / 50%);

        &::before {
          content: '';
          position: fixed;
          inset: -100vh;
        }

        &.tui-enter,
        &.tui-leave {
          animation-name: tuiSlide, tuiFade;
        }
      }

      &.tui-enter,
      &.tui-leave {
        animation-name: tuiSlide;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [FormsModule, TuiButton, ResizerComponent, TuiPopup, TuiAnimated],
})
export class PluginsComponent {
  protected readonly mobile = inject(WA_IS_MOBILE)
  protected readonly service = inject(PluginsService)

  protected onClick(layerX: number) {
    if (layerX < 0 && this.mobile) {
      this.service.enabled.set(false)
    }
  }
}
