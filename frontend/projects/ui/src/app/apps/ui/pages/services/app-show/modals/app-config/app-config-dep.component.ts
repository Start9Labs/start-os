import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
} from '@angular/core'
import { compare, getValueByPointer, Operation } from 'fast-json-patch'
import { isObject } from '@start9labs/shared'
import { tuiIsNumber } from '@taiga-ui/cdk'

@Component({
  selector: 'app-config-dep',
  template: `
    <tui-notification>
      <h3 style="margin: 0 0 0.5rem; font-size: 1.25rem;">
        {{ package }}
      </h3>
      The following modifications have been made to {{ package }} to satisfy
      {{ dep }}:
      <ul>
        <li *ngFor="let d of diff" [innerHTML]="d"></li>
      </ul>
      To accept these modifications, click "Save".
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppConfigDepComponent implements OnChanges {
  @Input()
  package = ''

  @Input()
  dep = ''

  @Input()
  original: object = {}

  @Input()
  value: object = {}

  diff: string[] = []

  ngOnChanges() {
    this.diff = compare(this.original, this.value).map(
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
    const val = getValueByPointer(this.original, path)
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
