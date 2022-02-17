import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { LoadingController, ModalController } from '@ionic/angular'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { BasicInfo, getBasicInfoSpec } from './form-info'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { takeUntil } from 'rxjs/operators'
import { DevProjectData } from 'src/app/services/patch-db/data-model'
import { DestroyService } from '../../../../../../shared/src/services/destroy.service'
import * as yaml from 'js-yaml'

@Component({
  selector: 'developer-menu',
  templateUrl: 'developer-menu.page.html',
  styleUrls: ['developer-menu.page.scss'],
  providers: [DestroyService],
})
export class DeveloperMenuPage {
  yamlToPreview = ''
  projectId: string
  projectData: DevProjectData
  editorOptions = { theme: 'vs-dark', language: 'yaml' }

  constructor(
    private readonly route: ActivatedRoute,
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly destroy$: DestroyService,
    public readonly patchDb: PatchDbService,
  ) {}

  ngOnInit() {
    this.projectId = this.route.snapshot.paramMap.get('projectId')

    this.patchDb
      .watch$('ui', 'dev', this.projectId)
      .pipe(takeUntil(this.destroy$))
      .subscribe(pd => {
        this.projectData = pd
      })
  }

  setYaml(obj: {}, event: Event) {
    event.stopPropagation()
    this.yamlToPreview = yaml.dump(obj)
  }

  async openBasicInfoModal() {
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'Basic Info',
        spec: getBasicInfoSpec(this.projectData),
        buttons: [
          {
            text: 'Save',
            handler: (basicInfo: BasicInfo) => {
              this.saveBasicInfo(basicInfo)
            },
            isSubmit: true,
          },
        ],
      },
    })
    await modal.present()
  }

  async saveBasicInfo(basicInfo: BasicInfo) {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Saving...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.api.setDbValue({
        pointer: `/dev/${this.projectId}/basic-info`,
        value: basicInfo,
      })
    } catch (e) {
      this.errToast.present({
        message: 'Save error:  Your changes are not saved.',
      } as any)
    } finally {
      loader.dismiss()
    }
  }
}
