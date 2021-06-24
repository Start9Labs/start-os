import { Injectable } from '@angular/core'
import { AvailableShow } from 'src/app/services/api/api-types'
import { ApiService } from 'src/app/services/api/api.service'

@Injectable({
  providedIn: 'root',
})
export class AppAvailableService {
  pkgs: { [id: string]: AvailableShow } = { }

  constructor (
    private readonly apiService: ApiService,
  ) { }

  async setPkg (id: string, version?: string): Promise<void> {
    this.pkgs[id] = await this.apiService.getAvailableShow({ id, version })
  }
}


