import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ModalController } from '@ionic/angular'
import { debounce, ErrorService } from '@start9labs/shared'
import * as yaml from 'js-yaml'
import { filter, take } from 'rxjs/operators'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import { getProjectId } from 'src/app/util/get-project-id'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { FormDialogService } from '../../../services/form-dialog.service'
import { FormPage } from '../../../modals/form/form.page'

@Component({
  selector: 'dev-config',
  templateUrl: 'dev-config.page.html',
  styleUrls: ['dev-config.page.scss'],
})
export class DevConfigPage {
  readonly projectId = getProjectId(this.route)
  editorOptions = { theme: 'vs-dark', language: 'yaml' }
  code: string = ''
  saving: boolean = false

  constructor(
    private readonly formDialog: FormDialogService,
    private readonly errorHandler: ErrorService,
    private readonly route: ActivatedRoute,
    private readonly modalCtrl: ModalController,
    private readonly patch: PatchDB<DataModel>,
    private readonly api: ApiService,
  ) {}

  ngOnInit() {
    this.patch
      .watch$('ui', 'dev', this.projectId, 'config')
      .pipe(filter(Boolean), take(1))
      .subscribe(config => {
        this.code = config
      })
  }

  async preview() {
    let doc: any
    try {
      doc = yaml.load(this.code)
    } catch (e: any) {
      this.errorHandler.handleError(e)
    }

    this.formDialog.open(FormPage, {
      label: 'Config Sample',
      data: {
        spec: JSON.parse(JSON.stringify(doc, null, 2)),
        buttons: [
          {
            text: 'OK',
            handler: async () => true,
          },
        ],
      },
    })
  }

  @debounce(1000)
  async save() {
    this.saving = true
    try {
      await this.api.setDbValue<string>(
        ['dev', this.projectId, 'config'],
        this.code,
      )
    } catch (e: any) {
      this.errorHandler.handleError(e)
    } finally {
      this.saving = false
    }
  }
}
