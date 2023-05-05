import { Component } from '@angular/core'
import {
  TUI_FIRST_DAY,
  TUI_LAST_DAY,
  TuiDay,
  tuiPure,
  TuiTime,
} from '@taiga-ui/cdk'
import { ValueSpecDatetime } from 'start-sdk/lib/config/configTypes'
import { Control } from '../control'

@Component({
  selector: 'form-datetime',
  templateUrl: './form-datetime.component.html',
})
export class FormDatetimeComponent extends Control<ValueSpecDatetime, string> {
  readonly min = TUI_FIRST_DAY
  readonly max = TUI_LAST_DAY

  @tuiPure
  getTime(value: string | null) {
    return value ? TuiTime.fromString(value) : null
  }

  getLimit(limit: string): [TuiDay, TuiTime] {
    return [
      TuiDay.jsonParse(limit.slice(0, 10)),
      limit.length === 10
        ? new TuiTime(0, 0)
        : TuiTime.fromString(limit.slice(-5)),
    ]
  }
}
