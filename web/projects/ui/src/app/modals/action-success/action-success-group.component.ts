import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiFadeModule, TuiTitleModule } from '@taiga-ui/experimental'
import { TuiAccordionModule } from '@taiga-ui/kit'
import { ActionSuccessItemComponent } from './action-success-item.component'

@Component({
  standalone: true,
  selector: 'app-action-success-group',
  template: `
    <p *ngFor="let item of value?.value">
      <app-action-success-item
        *ngIf="isSingle(item)"
        [value]="item"
      ></app-action-success-item>
      <tui-accordion-item *ngIf="!isSingle(item)" size="s">
        <div tuiFade>{{ item.name }}</div>
        <ng-template tuiAccordionItemContent>
          <app-action-success-group [value]="item"></app-action-success-group>
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
    ActionSuccessItemComponent,
    TuiAccordionModule,
    TuiFadeModule,
  ],
})
export class ActionSuccessGroupComponent {
  @Input()
  value?: T.ActionResultV1 & { type: 'group' }

  isSingle(
    value: T.ActionResultV1,
  ): value is T.ActionResultV1 & { type: 'value' | 'message' } {
    return value.type === 'value' || value.type === 'message'
  }
}
