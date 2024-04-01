import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'

@Component({
  selector: 'app-show-progress',
  templateUrl: './app-show-progress.component.html',
  styleUrls: ['./app-show-progress.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowProgressComponent {
  @Input()
  phases!: T.FullProgress['phases']
}
