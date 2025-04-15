import { ChangeDetectionStrategy, Component } from '@angular/core'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiLoader } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'

@Component({
  standalone: true,
  template: '<tui-loader [textContent]="content | i18n" />',
  styleUrls: ['./loading.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader, i18nPipe],
})
export class LoadingComponent {
  readonly content = injectContext<{ content: i18nKey }>().content
}
