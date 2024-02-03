import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  template: 'Here be snek',
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class SnekComponent {}
