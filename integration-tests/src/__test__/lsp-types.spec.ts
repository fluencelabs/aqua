// FIXME: these tests work only with LSP ESM build (ModuleKind.ESModule in build.sbt)
/*
import {
    AbilityType,
    AquaLSP, ArrayType, BottomType,
    OptionType,
    ScalarType, StreamType, StructType,
    TopType,
    Type, ServiceType, ArrowType
} from '@fluencelabs/aqua-language-server-api/aqua-lsp-api';

describe("Testing LSP types", () => {
    it("range", async () => {
        const compiled = await AquaLSP.compile("lsp-aqua/types.aqua", {})
        const types = compiled.tokens.map(ti => ti.type)

        const isTop = (type: Type): type is TopType => type.tag === "top";
        const tops = types.filter(isTop)
        expect(tops).toHaveLength(1)

        const isBottom = (type: Type): type is BottomType => type.tag === "bottom";
        const bottom = types.filter(isBottom)
        expect(bottom).toHaveLength(1)

        const isScalar = (type: Type): type is ScalarType => type.tag === "scalar";
        const scalars = types.filter(isScalar)
        expect(scalars).toHaveLength(8)
        scalars.forEach(sc => expect(sc.name).toBeDefined())

        const isArray = (type: Type): type is ArrayType => type.tag === "array";
        const arrays = types.filter(isArray)
        expect(arrays).toHaveLength(2)
        arrays.forEach(sc => expect(sc.element).toBeDefined())

        const isOption = (type: Type): type is OptionType => type.tag === "option";
        const options = types.filter(isOption)
        expect(options).toHaveLength(2)
        options.forEach(sc => expect(sc.element).toBeDefined())

        const isStream = (type: Type): type is StreamType => type.tag === "stream";
        const streams = types.filter(isStream)
        expect(streams).toHaveLength(1)
        streams.forEach(sc => expect(sc.element).toBeDefined())

        const isAbility = (type: Type): type is AbilityType => type.tag === "ability";
        const abilities = types.filter(isAbility)
        expect(abilities).toHaveLength(1)
        abilities.forEach((sc) => {
            expect(sc.name).toBeDefined()
            expect(sc.fields).toBeDefined()
            expect(Object.entries(sc.fields)).toHaveLength(2)
        })

        const isStruct = (type: Type): type is StructType => type.tag === "struct";
        const structs = types.filter(isStruct)
        expect(structs).toHaveLength(1)
        structs.forEach((sc) => {
            expect(sc.name).toBeDefined()
            expect(sc.fields).toBeDefined()
            expect(Object.entries(sc.fields)).toHaveLength(4)
        })

        const isService = (type: Type): type is ServiceType => type.tag === "service";
        const services = types.filter(isService)
        expect(services).toHaveLength(1)
        services.forEach((sc) => {
            expect(sc.name).toBeDefined()
            expect(sc.fields).toBeDefined()
            expect(Object.entries(sc.fields)).toHaveLength(1)
        })

        const isArrow = (type: Type): type is ArrowType => type.tag === "arrow";
        const arrows = types.filter(isArrow)
        expect(arrows).toHaveLength(3)

        const fullArrow = arrows[0]
        const argA = fullArrow.domain.args["a"]
        expect(argA).toEqual({name: "string", tag: "scalar"})

        const argB = fullArrow.domain.args["b"]
        expect(argB).toEqual({name: "u32", tag: "scalar"})

        const codomain = fullArrow.codomain.types
        expect(codomain[0]).toEqual({name: "string", tag: "scalar"})
        expect(codomain[1]).toEqual({name: "u32", tag: "scalar"})

        const nilArrow = arrows[1]
        expect(nilArrow.domain.args).toEqual({})
        expect(nilArrow.codomain.types).toEqual([])

        const srvNoopArrow = arrows[2]
        expect(srvNoopArrow.domain.args).toEqual({srvArg: {name: "string", tag: "scalar"}})
        expect(srvNoopArrow.codomain.types).toEqual([])

    })

})*/
