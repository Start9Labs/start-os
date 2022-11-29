import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'app-loading',
  templateUrl: 'loading.page.html',
  styleUrls: ['loading.page.scss'],
})
export class LoadingPage {
  readonly progress$ = this.stateService.dataProgress$

  constructor(
    private readonly stateService: StateService,
    private readonly navCtrl: NavController,
  ) {}

  ngOnInit() {
    this.stateService.pollDataTransferProgress()
    const progSub = this.stateService.dataCompletionSubject$.subscribe(
      async complete => {
        if (complete) {
          progSub.unsubscribe()
          await this.navCtrl.navigateForward(`/success`)
        }
      },
    )
  }
}

import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'toMessage',
})
export class ToMessagePipe implements PipeTransform {
  constructor(private readonly stateService: StateService) {}

  transform(progress: number | null): string {
    switch (this.stateService.setupType) {
      case 'fresh':
      case 'attach':
        return 'Setting up your Embassy'
      case 'restore':
        return 'Restoring data. This can take a while.'
      case 'transfer':
        if (!progress) {
          return 'Preparing data. Depending on how much data you have, this could take up to 1 hour'
        } else {
          return 'Transferring data'
        }
      default:
        return ''
    }
  }
}
