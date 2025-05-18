import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiAccordion, TuiFade } from '@taiga-ui/kit'
import { ActionSuccessMemberComponent } from './action-success-member.component'
import { GroupResult } from './types'

@Component({
  standalone: true,
  selector: 'app-action-success-group',
  template: `
    @for (member of group.value; track $index) {
      <p>
        @if (member.type === 'single') {
          <app-action-success-member [member]="member" />
        }
        @if (member.type === 'group') {
          <tui-accordion-item>
            <div tuiFade>{{ member.name }}</div>
            <ng-template tuiAccordionItemContent>
              <app-action-success-group [group]="member" />
            </ng-template>
          </tui-accordion-item>
        }
      </p>
    }
  `,
  styles: [
    `
      p:first-child {
        margin-top: 0;
      }

      p:last-child {
        margin-bottom: 0;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ActionSuccessMemberComponent, TuiAccordion, TuiFade],
})
export class ActionSuccessGroupComponent {
  @Input()
  group!: GroupResult
}
