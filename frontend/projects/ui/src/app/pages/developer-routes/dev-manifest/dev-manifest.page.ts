import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import * as yaml from 'js-yaml'
import { take } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { getProjectId } from 'src/app/util/get-project-id'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'dev-manifest',
  templateUrl: 'dev-manifest.page.html',
  styleUrls: ['dev-manifest.page.scss'],
})
export class DevManifestPage {
  readonly projectId = getProjectId(this.route)
  editorOptions = { theme: 'vs-dark', language: 'yaml', readOnly: true }
  manifest: string = ''

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  ngOnInit() {
    this.patch
      .watch$('ui', 'dev', this.projectId)
      .pipe(take(1))
      .subscribe(devData => {
        this.manifest = yaml.dump(devData['basic-info'])
      })
  }
}
