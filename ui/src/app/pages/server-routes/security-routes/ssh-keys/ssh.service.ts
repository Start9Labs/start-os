import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { SSHKeys } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class SSHService {
  private readonly keys$ = new BehaviorSubject<SSHKeys>({ })

  constructor (
    private readonly embassyApi: ApiService,
  ) { }

  watch$ () {
    return this.keys$.asObservable()
  }


}
