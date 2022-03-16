export abstract class AbstractApiService {
  // for getting static files: ex icons, instructions, licenses
  abstract getStatic(url: string): Promise<string>
}
