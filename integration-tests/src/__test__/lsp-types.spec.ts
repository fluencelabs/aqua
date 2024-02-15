import { AquaLSP, ErrorInfo, TokenLink, WarningInfo } from '@fluencelabs/aqua-language-server-api/aqua-lsp-api';

describe("Testing LSP types", () => {
    it("range", async () => {
        const compiled = await AquaLSP.compile("../../lsp-aqua/types.aqua", {})
        console.log(compiled.tokens)
    })

})