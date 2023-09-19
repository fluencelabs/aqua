#! /usr/bin/env node

const fs = require("fs").promises;
const path = require("path");

function printUsage() {
    console.log(
        `Usage: "ci check-consistency" or "ci bump-version %postfix%" or "ci get-version"`
    );
}

let postfix;
const mode = process.argv[2];

function validateArgs() {
    switch (mode) {
        case "get-version":
            return true;

        case "bump-version":
            postfix = process.argv[3];
            if (!postfix) {
                printUsage();
                process.exit();
            }
            return true;

        case "":
        case undefined:
        case "check-consistency":
            return true;

        default:
            return false;
    }
}

const PATHS_TO_PACKAGES = [
    "./api/api-npm",
    "./language-server/language-server-npm",
    "./integration-tests"
];

async function getPackageJsonsRecursive(currentPath) {
    return (
        await Promise.all(
            (await fs.readdir(currentPath, { withFileTypes: true }))
                .filter(
                    (file) =>
                        file.name !== "node_modules" && file.name !== "@tests" &&
                        (file.isDirectory() || file.name === "package.json")
                )
                .map((file) =>
                    file.isDirectory()
                        ? getPackageJsonsRecursive(
                              path.join(currentPath, file.name)
                          )
                        : Promise.resolve([
                              path.join(process.cwd(), currentPath, file.name),
                          ])
                )
        )
    ).flat();
}

async function getVersion(file) {
    const content = await fs.readFile(file);
    const json = JSON.parse(content);
    return [json.name, json.version];
}

function processDep(obj, name, fn) {
    if (!obj) {
        return;
    }

    if (!obj[name]) {
        return;
    }

    fn(obj, obj[name]);
}

async function getVersionsMap(allPackageJsons) {
    return new Map(await Promise.all(allPackageJsons.map(getVersion)));
}

function getVersionForPackageOrThrow(versionsMap, packageName) {
    const version = versionsMap.get(packageName);
    if (!version) {
        console.log("Failed to get version for package: ", packageName);
        process.exit(1);
    }
    return version;
}

async function checkConsistency(file, versionsMap) {
    console.log("Checking: ", file);
    const content = await fs.readFile(file);
    const json = JSON.parse(content);

    for (const [name, versionInDep] of versionsMap) {
        const check = (x, version) => {
            if (version.includes("*")) {
                return;
            }

            if (versionInDep !== version) {
                console.log(
                    `Error, versions don't match: ${name}:${version} !== ${versionInDep}`,
                    file
                );
                process.exit(1);
            }
        };
        processDep(json.dependencies, name, check);
        processDep(json.devDependencies, name, check);
    }
}

async function bumpVersions(file, versionsMap) {
    console.log("Updating: ", file);
    const content = await fs.readFile(file);
    const json = JSON.parse(content);

    for (const [name, version] of versionsMap) {
        const update = (x) => (x[name] = `${version}-${postfix}`);
        processDep(json.dependencies, name, update);
        processDep(json.devDependencies, name, update);
    }

    const version = getVersionForPackageOrThrow(versionsMap, json.name);
    json.version = `${version}-${postfix}`;

    const newContent = JSON.stringify(json, undefined, 4) + "\n";
    await fs.writeFile(file, newContent);
}

async function processPackageJsons(allPackageJsons, versionsMap, fn) {
    await Promise.all(allPackageJsons.map((x) => fn(x, versionsMap)));
}

async function run() {
    if (!validateArgs()) {
        printUsage();
        process.exit(0);
    }

    const packageJsons = (
        await Promise.all(PATHS_TO_PACKAGES.map(getPackageJsonsRecursive))
    ).flat();
    const versionsMap = await getVersionsMap(packageJsons);

    if (mode === "get-version") {
        const fjs = versionsMap.get("@fluencelabs/fluence");
        console.log(fjs);
        return;
    }

    console.log("Checking versions consistency...");
    await processPackageJsons(packageJsons, versionsMap, checkConsistency);
    console.log("Versions are consistent");

    if (mode === "bump-version") {
        console.log("Adding postfix: ", postfix);
        await processPackageJsons(packageJsons, versionsMap, bumpVersions);
        console.log("Done");
    }
}

run();
