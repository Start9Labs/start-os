import { inject, Injectable } from '@angular/core'
import { TuiAlertService } from '@taiga-ui/core'
import { copyToClipboard } from '../util/copy-to-clipboard'

@Injectable({ providedIn: 'root' })
export class CopyService {
  private readonly alerts = inject(TuiAlertService)

  async copy(text: string) {
    const success = await copyToClipboard(text)

    this.alerts
      .open(success ? 'Copied to clipboard!' : 'Failed to copy to clipboard.')
      .subscribe()
  }
}
