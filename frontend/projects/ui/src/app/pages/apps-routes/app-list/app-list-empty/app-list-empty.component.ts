import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'app-list-empty',
  templateUrl: 'app-list-empty.component.html',
  styleUrls: ['app-list-empty.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListEmptyComponent {}
