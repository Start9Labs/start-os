import { ErrorHandler, inject, Injectable } from '@angular/core'
import { TuiAlertService, TuiNotification } from '@taiga-ui/core'
import { HttpError } from '../classes/http-error'

// TODO: Enable this as ErrorHandler
@Injectable({
  providedIn: 'root',
})
export class ErrorService extends ErrorHandler {
  private readonly alerts = inject(TuiAlertService)

  override handleError(error: HttpError | string, link?: string) {
    console.error(error)

    this.alerts
      .open(getErrorMessage(error, link), {
        label: 'Error',
        autoClose: false,
        status: TuiNotification.Error,
      })
      .subscribe()
  }
}

export function getErrorMessage(e: HttpError | string, link?: string): string {
  let message = ''

  if (typeof e === 'string') {
    message = e
  } else if (e.code === 0) {
    message =
      'Request Error. Your browser blocked the request. This is usually caused by a corrupt browser cache or an overly aggressive ad blocker. Please clear your browser cache and/or adjust your ad blocker and try again'
  } else if (!e.message) {
    message = 'Unknown Error'
    link = 'https://docs.start9.com/latest/support/faq'
  } else {
    message = e.message
  }

  return link
    ? `${message}<br /><br /><a href=${link} target="_blank" rel="noreferrer">Get Help</a>`
    : message
}
