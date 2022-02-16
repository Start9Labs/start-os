import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'developer-menu',
  templateUrl: 'developer-menu.page.html',
  styleUrls: ['developer-menu.page.scss'],
})
export class DeveloperMenuPage {
  projectId: string
  constructor(
    private readonly route: ActivatedRoute,
    public readonly patchDb: PatchDbService,
  ) {}

  ngOnInit() {
    this.projectId = this.route.snapshot.paramMap.get('projectId')
  }
}
