import {
  ChangeDetectionStrategy,
  Component,
  input,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiTable, TuiTableDirective } from '@taiga-ui/addon-table'
import { TuiAutoFocus } from '@taiga-ui/cdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiInputInline, TuiSwitch } from '@taiga-ui/kit'
import { Placeholder } from 'src/app/routes/home/components/placeholder'
import { Forwarding } from 'src/app/routes/home/routes/forwarding/service'
import { injectFormService } from 'src/app/services/form.service'

@Component({
  selector: '[forwardingTable]',
  template: `
    <thead tuiThead>
      <tr>
        <th tuiTh [style.width.rem]="4">Enabled</th>
        <th tuiTh [style.min-width.rem]="10">Label</th>
        <th tuiTh>Protocol</th>
        <th tuiTh [style.min-width.rem]="10">Target Device/IP</th>
        <th tuiTh>External Ports</th>
        <th tuiTh>Internal Ports</th>
        <th tuiTh [style.width.rem]="2"></th>
      </tr>
    </thead>
    <tbody>
      @for (item of forwardingTable(); track $index) {
        <tr>
          <td tuiTd>
            <input
              type="checkbox"
              size="s"
              tuiSwitch
              [(ngModel)]="item.enabled"
              (ngModelChange)="onToggle($index)"
            />
          </td>
          <td tuiTd>
            @if (editing() === $index) {
              <tui-input-inline>
                <input
                  #input
                  tuiAutoFocus
                  [ngModelOptions]="{ updateOn: 'submit' }"
                  [(ngModel)]="item.purpose"
                  (keydown.escape)="editing.set(-1)"
                  (keydown.enter)="save(input.value, $index)"
                />
              </tui-input-inline>
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.check"
                appearance="action"
                class="g-positive"
                (click)="save(input.value, $index)"
              >
                Save
              </button>
            } @else {
              {{ item.purpose }}
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.pencil"
                appearance="icon"
                (click)="editing.set($index)"
              >
                Edit
              </button>
            }
          </td>
          <td tuiTd>{{ item.protocol }}</td>
          <td tuiTd>
            <div class="target">
              @if (deviceNames()[item.ip]; as name) {
                <span class="name">{{ name }}</span>
              }
              <span class="ip" [class.secondary]="deviceNames()[item.ip]">
                {{ item.ip }}
              </span>
            </div>
          </td>
          <td tuiTd>{{ item.external }}</td>
          <td tuiTd>{{ item.internal }}</td>
          <td tuiTd>
            <button
              tuiIconButton
              size="xs"
              iconStart="@tui.trash"
              appearance="icon"
              (click)="delete($index)"
            >
              Remove
            </button>
          </td>
        </tr>
      } @empty {
        <tr>
          <td tuiTd colspan="7">
            <app-placeholder icon="@tui.chevrons-right">
              No port forwarding rules have been set up
            </app-placeholder>
          </td>
        </tr>
      }
    </tbody>
  `,
  styles: `
    [tuiIconButton] {
      top: -0.125rem;
      margin-block: -0.25rem;
      margin-inline: 0.25rem;
    }

    [tuiSwitch] {
      display: flex;
      margin: 0.125rem 0 0;
    }

    tui-input-inline {
      width: calc(100% - 2rem);
    }

    .target {
      display: flex;
      flex-direction: column;

      .name {
        font-weight: 500;
      }

      .secondary {
        color: var(--tui-text-secondary);
        font: var(--tui-font-text-s);
      }
    }
  `,
  hostDirectives: [TuiTableDirective],
  host: { class: 'g-table' },
  imports: [
    FormsModule,
    TuiTable,
    TuiSwitch,
    TuiButton,
    Placeholder,
    TuiInputInline,
    TuiAutoFocus,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ForwardingTable {
  protected readonly service = injectFormService<Forwarding[]>()
  protected readonly editing = signal(NaN)

  public readonly forwardingTable = input<Forwarding[]>([])
  public readonly deviceNames = input<Record<string, string>>({})

  protected onToggle(index: number) {
    // item.enabled already mutated by [(ngModel)]
    this.service.save([...this.forwardingTable()])
  }

  protected delete(index: number) {
    const items = this.forwardingTable().slice()
    items.splice(index, 1)
    this.service.save(items)
  }

  protected save(value: string, index: number) {
    if (value) {
      const items = this.forwardingTable()
      items[index].purpose = value
      this.service.save([...items])
    }
    this.editing.set(-1)
  }
}
