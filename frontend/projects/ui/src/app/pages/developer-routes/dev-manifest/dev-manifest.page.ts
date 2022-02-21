import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import * as yaml from 'js-yaml'
import { take } from 'rxjs/operators'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'dev-manifest',
  templateUrl: 'dev-manifest.page.html',
  styleUrls: ['dev-manifest.page.scss'],
})
export class DevManifestPage {
  projectId: string
  editorOptions = { theme: 'vs-dark', language: 'yaml', readOnly: true }
  manifest: string = ''

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patchDb: PatchDbService,
  ) {}

  ngOnInit() {
    this.projectId = this.route.snapshot.paramMap.get('projectId')

    this.patchDb
      .watch$('ui', 'dev', this.projectId)
      .pipe(take(1))
      .subscribe(devData => {
        this.manifest = yaml.dump(devData['basic-info'])
      })
  }
}
