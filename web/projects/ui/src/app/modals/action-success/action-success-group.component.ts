import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiFadeModule, TuiTitleModule } from '@taiga-ui/experimental'
import { TuiAccordionModule } from '@taiga-ui/kit'
import { ActionSuccessMemberComponent } from './action-success-member.component'
import { GroupResult } from './types'

@Component({
  standalone: true,
  selector: 'app-action-success-group',
  template: `
    <p *ngFor="let member of group.value">
      <app-action-success-member
        *ngIf="member.type === 'single'"
        [member]="member"
      ></app-action-success-member>
      <tui-accordion-item *ngIf="member.type === 'group'">
        <div tuiFade>{{ member.name }}</div>
        <ng-template tuiAccordionItemContent>
          <app-action-success-group [group]="member"></app-action-success-group>
        </ng-template>
      </tui-accordion-item>
    </p>
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
  imports: [
    CommonModule,
    TuiTitleModule,
    ActionSuccessMemberComponent,
    TuiAccordionModule,
    TuiFadeModule,
  ],
})
export class ActionSuccessGroupComponent {
  @Input()
  group!: GroupResult
}
