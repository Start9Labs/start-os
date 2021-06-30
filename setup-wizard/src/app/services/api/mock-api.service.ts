import { Injectable } from "@angular/core";
import { pauseFor } from "../state.service";
import { ApiService } from "./api.service"

@Injectable({
  providedIn: 'root'
})
export class MockApiService extends ApiService {

  constructor () {
    super()
  }

  async getState () {
    return {
      'selected-data-drive': 'page1',
      'recovery-drive': null,
      'has-password': false,
      'data-transfer-progress': null
    }
  }

  async getDataTransferProgress () {
    tries = Math.min(tries + 1, 4)
    return {
      'bytes-transfered': tries,
      'total-bytes': 4
    }
  }

  async getStorageDisks () {
    await pauseFor(2000)
    return [
      {
        "logical-name": 'name1',
        labels: ['label 1', 'label 2'],
        capacity: 1600,
        used: 200,
      },
      {
        "logical-name": 'name2',
        labels: [],
        capacity: 1600,
        used: 0,
      }
    ]
  }

  async selectStorageDisk(disk) {
    await pauseFor(200)
    return
  }

  async getEmbassyDrives () {
    await pauseFor(2000)
    return [
      {
        "logical-name": 'name1',
        version: '0.2.3',
        name: 'Embassy 0.2.3'
      }
    ]
  }

  async selectEmbassyDrive(disk) {
    await pauseFor(200)
    return
  }

  async submitPassword(password) {
    await pauseFor(200)
    return
  }
}

let tries = 2