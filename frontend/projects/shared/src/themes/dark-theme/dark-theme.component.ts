import {
  ChangeDetectionStrategy,
  Component,
  ViewEncapsulation,
} from '@angular/core'
import { AbstractTuiThemeSwitcher } from '@taiga-ui/cdk'

@Component({
  selector: 'dark-theme',
  template: '',
  styleUrls: ['./dark-theme.component.scss'],
  encapsulation: ViewEncapsulation.None,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DarkThemeComponent extends AbstractTuiThemeSwitcher {}
