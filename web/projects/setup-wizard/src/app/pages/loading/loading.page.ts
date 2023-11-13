import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'
import { Pipe, PipeTransform } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'
import { ErrorToastService, pauseFor } from '@start9labs/shared'

type Progress = {
  totalBytes: number | null
  transferred: number
}

@Component({
  selector: 'app-loading',
  templateUrl: 'loading.page.html',
  styleUrls: ['loading.page.scss'],
})
export class LoadingPage {
  readonly progress$ = new BehaviorSubject<Progress>({
    totalBytes: null,
    transferred: 0,
  })

  constructor(
    private readonly navCtrl: NavController,
    private readonly api: ApiService,
    private readonly errorToastService: ErrorToastService,
  ) {}

  ngOnInit() {
    this.poll()
  }

  async poll() {
    try {
      const progress = await this.api.getStatus()

      if (!progress) return

      const {
        'total-bytes': totalBytes,
        'bytes-transferred': bytesTransferred,
      } = progress

      this.progress$.next({
        totalBytes,
        transferred: totalBytes ? bytesTransferred / totalBytes : 0,
      })

      if (progress.complete) {
        this.navCtrl.navigateForward(`/success`)
        this.progress$.complete()
        return
      }

      await pauseFor(250)

      setTimeout(() => this.poll(), 0) // prevent call stack from growing
    } catch (e: any) {
      this.errorToastService.present(e)
    }
  }
}

@Pipe({
  name: 'toMessage',
})
export class ToMessagePipe implements PipeTransform {
  constructor(private readonly stateService: StateService) {}

  transform(progress: number | null): string {
    if (['fresh', 'attach'].includes(this.stateService.setupType || '')) {
      return 'Setting up your server'
    }

    if (!progress) {
      return 'Calculating size'
    } else if (progress < 1) {
      return 'Copying data'
    } else {
      return 'Finalizing'
    }
  }
}
