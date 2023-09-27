import {
  concat_foobars,
  registerStringService,
} from "../compiled/examples/imports_exports/imports.js";
import { registerMyExportSrv } from "../compiled/examples/imports_exports/exports.js";
import { registerSuperFoo } from "../compiled/examples/imports_exports/declare.js";

export async function declareCall() {
  registerSuperFoo({
    small_foo: () => {
      return "small_foo";
    },
  });

  registerStringService({
    concat: (a, b) => {
      return a + b;
    },
  });
  registerMyExportSrv({
    another_str: () => {
      return "str_from_my_export_srv";
    },
  });
  return await concat_foobars();
}
