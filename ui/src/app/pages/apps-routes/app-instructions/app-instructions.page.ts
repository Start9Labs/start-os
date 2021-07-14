import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent } from '@ionic/angular'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'
import { ApiService } from 'src/app/services/api/api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'app-instructions',
  templateUrl: './app-instructions.page.html',
  styleUrls: ['./app-instructions.page.scss'],
})
export class AppInstructionsPage {
  instructions: string
  loading = true

  @ViewChild(IonContent) content: IonContent

  constructor (
    private readonly route: ActivatedRoute,
    private readonly errToast: ErrorToastService,
    private readonly apiService: ApiService,
    private readonly patch: PatchDbModel,
  ) { }

  async ngOnInit () {
    const pkgId = this.route.snapshot.paramMap.get('pkgId')
    const url = this.patch.data['package-data'][pkgId]['static-files'].instructions
    try {
      this.instructions = await this.apiService.getStatic(url)
    } catch (e) {
      console.error(e)
      this.errToast.present(e.message)
    } finally {
      this.loading = false
    }
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }
}
