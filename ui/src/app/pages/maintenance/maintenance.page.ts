import { Component } from '@angular/core'
import { ServerStatus } from 'src/app/models/patch-db/data-model'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

@Component({
  selector: 'Maintenance',
  templateUrl: 'Maintenance.page.html',
  styleUrls: ['Maintenance.page.scss'],
})
export class MaintenancePage {
  ServerStatus = ServerStatus

  constructor (
    public readonly patch: PatchDbModel,
  ) { }
}

