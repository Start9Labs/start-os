import { System } from "../../Interfaces/System"
import { SystemForEmbassy } from "./SystemForEmbassy"
import { SystemForStartOs } from "./SystemForStartOs"
export async function getSystem(): Promise<System> {
  return SystemForEmbassy.of()
}
