import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiAccordion, TuiFade } from '@taiga-ui/kit'
import { ActionSuccessMemberComponent } from './action-success-member.component'
import { GroupResult } from './types'

@Component({
  selector: 'app-action-success-group',
  template: `
    @for (member of group.value; track $index) {
      <p>
        @if (member.type === 'single') {
          <app-action-success-member [member]="member" />
        }
        @if (member.type === 'group') {
          <tui-accordion>
            <button tuiAccordion>
              <span tuiFade>{{ member.name }}</span>
            </button>
            <tui-expand>
              <app-action-success-group [group]="member" />
            </tui-expand>
          </tui-accordion>
        }
      </p>
    }
  `,
  styles: `
    p:first-child {
      margin-top: 0;
    }

    p:last-child {
      margin-bottom: 0;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ActionSuccessMemberComponent, TuiAccordion, TuiFade],
})
export class ActionSuccessGroupComponent {
  @Input()
  group!: GroupResult
}
