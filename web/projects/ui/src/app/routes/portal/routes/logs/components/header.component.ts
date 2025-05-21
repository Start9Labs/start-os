import {
  ChangeDetectionStrategy,
  Component,
  input,
  ViewEncapsulation,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { TitleDirective } from 'src/app/services/title.service'

@Component({
  standalone: true,
  selector: 'logs-header',
  template: `
    <ng-container *title>
      <a tuiIconButton size="m" iconStart="@tui.arrow-left" routerLink="..">
        {{ 'Back' | i18n }}
      </a>
      {{ title() }}
    </ng-container>
    <hgroup tuiTitle>
      <h3>{{ title() }}</h3>
      <p tuiSubtitle><ng-content /></p>
    </hgroup>
    <aside tuiAccessories>
      <a
        tuiIconButton
        appearance="secondary-grayscale"
        iconStart="@tui.x"
        size="s"
        routerLink=".."
        [style.border-radius.%]="100"
      >
        {{ 'Close' | i18n }}
      </a>
    </aside>
  `,
  styles: `
    logs-header[tuiHeader] {
      margin-block-end: 1rem;

      + logs {
        height: calc(100% - 5rem);
      }

      tui-root._mobile & {
        display: none;

        + logs {
          height: 100%;
        }
      }
    }
  `,
  encapsulation: ViewEncapsulation.None,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiTitle, i18nPipe, RouterLink, TitleDirective],
  hostDirectives: [TuiHeader],
})
export class LogsHeaderComponent {
  readonly title = input<string | undefined>()
}
