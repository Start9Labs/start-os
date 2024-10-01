import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnInit,
} from '@angular/core'
import { getValueByPointer, Operation } from 'fast-json-patch'
import { isObject } from '@start9labs/shared'
import { tuiIsNumber } from '@taiga-ui/cdk'
import { CommonModule } from '@angular/common'
import { TuiNotificationModule } from '@taiga-ui/core'

@Component({
  selector: 'action-dep',
  template: `
    <tui-notification>
      <h3 style="margin: 0 0 0.5rem; font-size: 1.25rem;">
        {{ pkgTitle }}
      </h3>
      The following modifications have been made to {{ pkgTitle }} to satisfy
      {{ depTitle }}:
      <ul>
        <li *ngFor="let d of diff" [innerHTML]="d"></li>
      </ul>
    </tui-notification>
  `,
  standalone: true,
  imports: [CommonModule, TuiNotificationModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ActionDepComponent implements OnInit {
  @Input()
  pkgTitle = ''

  @Input()
  depTitle = ''

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
        return `Added ${this.getNewValue(operation.value)}`
      case 'remove':
        return `Removed ${this.getOldValue(operation.path)}`
      case 'replace':
        return `Changed from ${this.getOldValue(
          operation.path,
        )} to ${this.getNewValue(operation.value)}`
      default:
        return `Unknown operation`
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
