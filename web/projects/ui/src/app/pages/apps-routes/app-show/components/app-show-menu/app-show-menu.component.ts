import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { Button } from '../../pipes/to-buttons.pipe'

@Component({
  selector: 'app-show-menu',
  templateUrl: './app-show-menu.component.html',
  styleUrls: ['./app-show-menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowMenuComponent {
  @Input()
  buttons: Button[] = []
}
