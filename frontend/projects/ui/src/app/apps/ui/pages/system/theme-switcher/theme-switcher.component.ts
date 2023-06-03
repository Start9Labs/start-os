import { ChangeDetectionStrategy, Component } from '@angular/core'

import { ThemeSwitcherService } from 'src/app/services/theme-switcher.service'

@Component({
  selector: 'theme-switcher',
  templateUrl: './theme-switcher.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ThemeSwitcherComponent {
  value = this.switcher.value

  open = false

  readonly themes = ['Dark', 'Light']

  constructor(private readonly switcher: ThemeSwitcherService) {}

  onChange(value: string): void {
    this.value = value
    this.switcher.next(value)
  }
}
