import { Component } from '@angular/core'
import { ServerStatus } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'maintenance',
  templateUrl: 'maintenance.page.html',
  styleUrls: ['maintenance.page.scss'],
})
export class MaintenancePage {
  ServerStatus = ServerStatus

  constructor (
    public readonly patch: PatchDbService,
  ) { }
}

