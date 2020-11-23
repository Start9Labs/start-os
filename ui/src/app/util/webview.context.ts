import WebviewContext from '@start9labs/ambassador-sdk/dist/webview-context'

export const webviewContext = new WebviewContext(async (method: string, data: any) => {
  throw new Error (`${method} UNIMPLEMENTED`)
  // switch(method){
  //   case 'getConfigValue': throw new Error ('getConfigValue UNIMPLEMENTED')
  // }
})