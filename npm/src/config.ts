
type Module = {
    name: string,
    path: string,
    mounted_binaries: string[][],
    logger_enabled: boolean[],
    preopened_files: string[],
    envs: string[][],
    mapped_dirs: string[],
    logging_mask: number[],
    mem_pages_count: number[],
    max_heap_size: string[]
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
        if (!m.envs) {
            m.envs = []
        }
        if (!m.logging_mask) {
            m.logging_mask = []
        }
        if (!m.mem_pages_count) {
            m.mem_pages_count = []
        }
        if (!m.max_heap_size) {
            m.max_heap_size = []
        }
        return m
    })
    return config
}
