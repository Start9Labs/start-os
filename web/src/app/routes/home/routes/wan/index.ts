import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'
import { Help } from 'src/app/directives/help.directive'

@Component({
  template: `
    @for (item of mock; track $index) {
      <p>{{ item }}</p>
    }
    <ng-template help>
      <tui-accordion size="m">
        <button tuiAccordion appearance="">IP Address</button>
        <tui-expand>192.168.1.1</tui-expand>

        <button tuiAccordion appearance="">DNS Server</button>
        <tui-expand>google.com</tui-expand>
      </tui-accordion>
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [Help, TuiAccordion],
})
export default class Wan {
  protected readonly mock = Array.from({ length: 200 }).fill('wan')
}
