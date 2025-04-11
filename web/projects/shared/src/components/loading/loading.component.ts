import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiLoader } from '@taiga-ui/core'
import { injectContext, PolymorpheusContent } from '@taiga-ui/polymorpheus'

@Component({
  standalone: true,
  template: '<tui-loader [textContent]="content" />',
  styleUrls: ['./loading.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader],
})
export class LoadingComponent {
  readonly content = injectContext<{ content: PolymorpheusContent }>().content
}
