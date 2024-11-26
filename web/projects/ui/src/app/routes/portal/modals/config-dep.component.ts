import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnInit,
} from '@angular/core'
import { TuiNotification } from '@taiga-ui/core'
import { getValueByPointer, Operation } from 'fast-json-patch'
import { isObject } from '@start9labs/shared'
import { tuiIsNumber } from '@taiga-ui/cdk'
import { CommonModule } from '@angular/common'

@Component({
  selector: 'action-request-info',
  template: `
    <tui-notification *ngIf="diff.length">
      The following modifications were made:
      <ul>
        <li *ngFor="let d of diff" [innerHTML]="d"></li>
      </ul>
    </tui-notification>
  `,
  standalone: true,
  imports: [CommonModule, TuiNotification],
  changeDetection: ChangeDetectionStrategy.OnPush,
  styles: [
    `
      tui-notification {
        margin-bottom: 1.5rem;
      }
    `,
  ],
})
export class ActionRequestInfoComponent implements OnInit {
  @Input()
  originalValue: object = {}

  @Input()
  operations: Operation[] = []

  diff: string[] = []

  ngOnInit() {
    this.diff = this.operations.map(
      op => `${this.getPath(op)}: ${this.getMessage(op)}`,
    )
  }

  private getPath(operation: Operation): string {
    const path = operation.path
      .substring(1)
      .split('/')
      .map(node => {
        const num = Number(node)
        return isNaN(num) ? node : num
      })

    if (tuiIsNumber(path[path.length - 1])) {
      path.pop()
    }

    return path.join(' &rarr; ')
  }

  private getMessage(operation: Operation): string {
    switch (operation.op) {
      case 'add':
        return `added ${this.getNewValue(operation.value)}`
      case 'remove':
        return `removed ${this.getOldValue(operation.path)}`
      case 'replace':
        return `changed from ${this.getOldValue(
          operation.path,
        )} to ${this.getNewValue(operation.value)}`
      default:
        return `Unknown operation` // unreachable
    }
  }

  private getOldValue(path: any): string {
    const val = getValueByPointer(this.originalValue, path)
    if (['string', 'number', 'boolean'].includes(typeof val)) {
      return val
    } else if (isObject(val)) {
      return 'entry'
    } else {
      return 'list'
    }
  }

  private getNewValue(val: any): string {
    if (['string', 'number', 'boolean'].includes(typeof val)) {
      return val
    } else if (isObject(val)) {
      return 'new entry'
    } else {
      return 'new list'
    }
  }
}
