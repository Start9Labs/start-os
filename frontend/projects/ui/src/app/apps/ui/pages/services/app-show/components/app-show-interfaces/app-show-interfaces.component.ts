import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
  ViewChild,
} from '@angular/core'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'

@Component({
  selector: 'app-show-interfaces',
  templateUrl: './app-show-interfaces.component.html',
  styleUrls: ['./app-show-interfaces.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowInterfacesComponent {
  @Input({ required: true }) pkgId!: string

  get interfaceInfo$() {
    return this.patch.watch$(
      'package-data',
      this.pkgId,
      'installed',
      'interfaceInfo',
    )
  }

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
