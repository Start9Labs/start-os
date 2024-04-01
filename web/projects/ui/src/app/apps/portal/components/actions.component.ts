import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiDataListModule } from '@taiga-ui/core'
import { TuiIconModule } from '@taiga-ui/experimental'

export interface Action {
  icon: string
  label: string
  action: () => void
}

@Component({
  selector: 'app-actions',
  template: `
    <tui-data-list>
      <h3 class="title"><ng-content /></h3>
      <tui-opt-group
        *ngFor="let group of actions | keyvalue: asIsOrder"
        [label]="group.key.toUpperCase()"
      >
        <button
          *ngFor="let action of group.value"
          tuiOption
          class="item"
          (click)="action.action()"
        >
          <tui-icon class="icon" [icon]="action.icon" />
          {{ action.label }}
        </button>
      </tui-opt-group>
    </tui-data-list>
  `,
  styles: [
    `
      .title {
        margin: 0;
        padding: 0 0.5rem 0.25rem;
        white-space: nowrap;
        font: var(--tui-font-text-l);
        font-weight: bold;
      }

      .item {
        justify-content: flex-start;
        gap: 0.75rem;
      }

      .icon {
        opacity: var(--tui-disabled-opacity);
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiDataListModule, CommonModule, TuiIconModule],
})
export class ActionsComponent {
  @Input()
  actions: Record<string, readonly Action[]> = {}

  asIsOrder(a: any, b: any) {
    return 0
  }
}
