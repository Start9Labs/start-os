import { Injectable } from '@angular/core'
import { Dump } from 'patch-db-client'
import { TunnelData } from '../patch-db/data-model'
import { Observable } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export abstract class ApiService {
  abstract openWebsocket$<T>(guid: string): Observable<T>
  abstract subscribe(): Promise<SubscribeRes> // db.subscribe
  // auth
  abstract login(params: LoginReq): Promise<null> // auth.login
  abstract logout(): Promise<null> // auth.logout
  abstract setPassword(params: LoginReq): Promise<null> // auth.set-password
  // subnets
  abstract addSubnet(params: UpsertSubnetReq): Promise<null> // subnet.add
  abstract editSubnet(params: UpsertSubnetReq): Promise<null> // subnet.add
  abstract deleteSubnet(params: DeleteSubnetReq): Promise<null> // subnet.remove
  // devices
  abstract addDevice(params: UpsertDeviceReq): Promise<null> // device.add
  abstract editDevice(params: UpsertDeviceReq): Promise<null> // device.add
  abstract deleteDevice(params: DeleteDeviceReq): Promise<null> // device.remove
  abstract showDeviceConfig(params: DeleteDeviceReq): Promise<string> // device.show-config
  // forwards
  abstract addForward(params: AddForwardReq): Promise<null> // port-forward.add
  abstract deleteForward(params: DeleteForwardReq): Promise<null> // port-forward.remove
}

export type SubscribeRes = {
  dump: Dump<TunnelData>
  guid: string
}

export type LoginReq = { password: string }

export type UpsertSubnetReq = {
  name: string
  subnet: string
}

export type DeleteSubnetReq = {
  subnet: string
}

export type UpsertDeviceReq = {
  name: string
  subnet: string
  ip: string
}

export type DeleteDeviceReq = {
  subnet: string
  ip: string
}

export type AddForwardReq = {
  source: string // externalip:port
  target: string // internalip:port
}

export type DeleteForwardReq = {
  source: string
}
