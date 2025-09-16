import { ChangeDetectionStrategy, Component } from '@angular/core'
import { Help } from 'src/app/directives/help.directive'

@Component({
  template: `
    @for (item of mock; track $index) {
      <p>{{ item }}</p>
    }
    <ng-template help>
      @for (item of mock; track $index) {
        <p>{{ item }}</p>
      }
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [Help],
})
export default class Outbound {
  protected readonly mock = Array.from({ length: 200 }).fill('outbound')
}
