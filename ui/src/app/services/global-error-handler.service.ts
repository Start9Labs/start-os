import { ErrorHandler, Injectable } from '@angular/core'

@Injectable()
export class GlobalErrorHandler implements ErrorHandler {

  handleError (e: any): void {
    console.error(e)
    const chunkFailedMessage = /Loading chunk [\d]+ failed/

    if (chunkFailedMessage.test(e.message)) {
      window.location.reload()
    }
  }
}