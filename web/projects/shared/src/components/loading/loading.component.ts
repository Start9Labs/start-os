import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiLoader } from '@taiga-ui/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusContent,
} from '@taiga-ui/polymorpheus'

@Component({
  standalone: true,
  template: '<tui-loader [textContent]="content" />',
  styleUrls: ['./loading.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader],
})
export class LoadingComponent {
  readonly content: PolymorpheusContent =
    inject(POLYMORPHEUS_CONTEXT)['content']
}
