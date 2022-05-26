import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ModalController } from '@ionic/angular'
import { filter, take } from 'rxjs/operators'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  debounce,
  exists,
  ErrorToastService,
  MarkdownComponent,
} from '@start9labs/shared'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { getProjectId } from 'src/app/util/get-project-id'

@Component({
  selector: 'dev-instructions',
  templateUrl: 'dev-instructions.page.html',
  styleUrls: ['dev-instructions.page.scss'],
})
export class DevInstructionsPage {
  readonly projectId = getProjectId(this.route)
  editorOptions = { theme: 'vs-dark', language: 'markdown' }
  code: string = ''
  saving: boolean = false

  constructor(
    private readonly route: ActivatedRoute,
    private readonly errToast: ErrorToastService,
    private readonly modalCtrl: ModalController,
    private readonly patchDb: PatchDbService,
    private readonly api: ApiService,
  ) {}

  ngOnInit() {
    this.patchDb
      .watch$('ui', 'dev', this.projectId, 'instructions')
      .pipe(filter(exists), take(1))
      .subscribe(config => {
        this.code = config
      })
  }

  async preview() {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Instructions Sample',
        content: this.code,
      },
      component: MarkdownComponent,
    })

    await modal.present()
  }

  @debounce(1000)
  async save() {
    this.saving = true
    try {
      await this.api.setDbValue({
        pointer: `/dev/${this.projectId}/instructions`,
        value: this.code,
      })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.saving = false
    }
  }
}
