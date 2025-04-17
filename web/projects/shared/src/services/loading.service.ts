import { ChangeDetectionStrategy, Component, Injectable } from '@angular/core'
import { TuiPopoverService } from '@taiga-ui/cdk'
import { TUI_DIALOGS, TuiLoader } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'
import { i18nPipe } from '../i18n/i18n.pipe'
import { i18nKey } from '../i18n/i18n.providers'

@Component({
  standalone: true,
  template: '<tui-loader [textContent]="content | i18n" />',
  styles: `
    :host {
      display: flex;
      align-items: center;
      max-width: 80%;
      margin: auto;
      padding: 1.5rem;
      background: var(--tui-background-elevation-1);
      border-radius: var(--tui-radius-m);
      box-shadow: var(--tui-shadow-popup);

      --tui-background-accent-1: var(--tui-status-warning);
    }

    tui-loader {
      flex-shrink: 0;
      min-width: 2rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader, i18nPipe],
})
class LoadingComponent {
  readonly content = injectContext<{ content: i18nKey }>().content
}

@Injectable({
  providedIn: `root`,
  useFactory: () => new LoadingService(TUI_DIALOGS, LoadingComponent),
})
export class LoadingService extends TuiPopoverService<unknown> {
  override open<G = void>(textContent: i18nKey) {
    return super.open<G>(textContent)
  }
}
