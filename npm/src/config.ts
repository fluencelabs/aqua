
type Module = {
    name: string,
    path: string,
    mounted_binaries: string[][],
    logger_enabled: boolean[],
    preopened_files: string[],
    mapped_dirs: string[]
}

type Config = {
    name: string,
    modules: Module[]
}

export function fillWithEmptyArrays(config: Config): Config {
    config.modules = config.modules.map((m) => {
        if (!m.logger_enabled) {
            m.logger_enabled = []
        }
        if (!m.mounted_binaries) {
            m.mounted_binaries = []
        }
        if (!m.preopened_files) {
            m.preopened_files = []
        }
        if (!m.mapped_dirs) {
            m.mapped_dirs = []
        }
        return m
    })
    return config
}
