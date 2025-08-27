import { Component } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { IST } from '@start9labs/start-sdk'
import {
  TUI_FIRST_DAY,
  TUI_LAST_DAY,
  TuiDay,
  TuiMapperPipe,
  tuiPure,
  TuiTime,
} from '@taiga-ui/cdk'
import { TuiIcon, TuiTextfield } from '@taiga-ui/core'
import {
  TuiInputDate,
  TuiInputDateTime,
  TuiInputTime,
  TuiTooltip,
} from '@taiga-ui/kit'

import { Control } from './control'
import { HintPipe } from '../pipes/hint.pipe'

@Component({
  selector: 'form-datetime',
  template: `
    <!--
      TODO: Move @switch down to only affect <input ... /> after fix:
      https://github.com/taiga-family/taiga-ui/issues/11780
    -->
    @switch (spec.inputmode) {
      @case ('time') {
        <tui-textfield (tuiActiveZoneChange)="!$event && control.onTouched()">
          @if (spec.name) {
            <label tuiLabel>
              {{ spec.name }}
              @if (spec.required) {
                <span>*</span>
              }
            </label>
          }
          <input
            tuiInputTime
            type="time"
            [invalid]="control.invalid()"
            [readOnly]="readOnly"
            [disabled]="!!spec.disabled"
            [ngModel]="getTime(value)"
            (ngModelChange)="value = $event?.toString() || null"
            (blur)="control.onTouched()"
          />
          @if (spec | hint; as hint) {
            <tui-icon [tuiTooltip]="hint" />
          }
        </tui-textfield>
      }
      @case ('date') {
        <tui-textfield (tuiActiveZoneChange)="!$event && control.onTouched()">
          @if (spec.name) {
            <label tuiLabel>
              {{ spec.name }}
              @if (spec.required) {
                <span>*</span>
              }
            </label>
          }
          <input
            tuiInputDate
            type="date"
            [invalid]="control.invalid()"
            [readOnly]="readOnly"
            [disabled]="!!spec.disabled"
            [min]="spec.min ? (spec.min | tuiMapper: getLimit)[0] : min"
            [max]="spec.max ? (spec.max | tuiMapper: getLimit)[0] : max"
            [(ngModel)]="value"
            (blur)="control.onTouched()"
          />
          @if (spec | hint; as hint) {
            <tui-icon [tuiTooltip]="hint" />
          }
          <tui-calendar *tuiTextfieldDropdown />
        </tui-textfield>
      }
      @case ('datetime-local') {
        <tui-textfield (tuiActiveZoneChange)="!$event && control.onTouched()">
          @if (spec.name) {
            <label tuiLabel>
              {{ spec.name }}
              @if (spec.required) {
                <span>*</span>
              }
            </label>
          }
          <input
            tuiInputDateTime
            type="datetime-local"
            [invalid]="control.invalid()"
            [readOnly]="readOnly"
            [disabled]="!!spec.disabled"
            [min]="spec.min ? (spec.min | tuiMapper: getLimit)[0] : min"
            [max]="spec.max ? (spec.max | tuiMapper: getLimit)[0] : max"
            [(ngModel)]="value"
            (blur)="control.onTouched()"
          />
          @if (spec | hint; as hint) {
            <tui-icon [tuiTooltip]="hint" />
          }
          <tui-calendar *tuiTextfieldDropdown />
        </tui-textfield>
      }
    }
  `,
  imports: [
    FormsModule,
    TuiTextfield,
    TuiIcon,
    TuiTooltip,
    TuiInputTime,
    TuiInputDate,
    TuiMapperPipe,
    TuiInputDateTime,
    HintPipe,
  ],
})
export class FormDatetimeComponent extends Control<
  IST.ValueSpecDatetime,
  string
> {
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
