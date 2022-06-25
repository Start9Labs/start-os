import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { ItemReorderEventDetail } from '@ionic/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-list-reorder',
  templateUrl: 'app-list-reorder.component.html',
  styleUrls: ['app-list-reorder.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListReorderComponent {
  @Input()
  reordering = false

  @Input()
  pkgs: readonly PackageDataEntry[] = []

  @Output()
  readonly reorderingChange = new EventEmitter<boolean>()

  @Output()
  readonly pkgsChange = new EventEmitter<readonly PackageDataEntry[]>()

  toggle() {
    this.reordering = !this.reordering
    this.reorderingChange.emit(this.reordering)
  }

  reorder({ detail }: CustomEvent<ItemReorderEventDetail>): void {
    this.pkgs = detail.complete([...this.pkgs])
    this.pkgsChange.emit(this.pkgs)
  }
}
