import { Injectable } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { ToastOptions } from '@ionic/core'
import { from, Observable } from 'rxjs'
import { tap } from 'rxjs/operators'

@Injectable({
  providedIn: 'root',
})
export class ToastService {
  constructor(private readonly toastCtrl: ToastController) {}

  create(options: ToastOptions): Observable<HTMLIonToastElement> {
    let toast = this.toastCtrl.create(options)

    return new Observable(subscriber => {
      const toast$ = from(toast).pipe(tap(toast => toast.present()))

      toast$.subscribe(subscriber)

      return
    })
  }
}
