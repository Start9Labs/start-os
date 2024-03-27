import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusContent,
} from '@tinkoff/ng-polymorpheus'

@Component({
  template: `
    <tui-loader [textContent]="content"></tui-loader>
  `,
  styleUrls: ['./loading.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LoadingComponent {
  readonly content: PolymorpheusContent =
    inject(POLYMORPHEUS_CONTEXT)['content']
}
