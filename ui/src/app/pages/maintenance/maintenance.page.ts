import { Component } from '@angular/core'
import { ServerStatus } from 'src/app/services/patch-db/data-model'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'

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

