#!/usr/bin/env bash
set -euo pipefail

packages=()
hlint_paths=()

is_unfinished() {
    grep -R -q --include='*.hs' 'error "You need to implement this function' "$1/src"
}

for directory in */; do
    [[ -d "$directory/src" ]] || continue

    if is_unfinished "$directory"; then
        echo "Skipping unfinished exercise: $directory" >&2
        continue
    fi

    hlint_paths+=("${directory}src")
done

while IFS= read -r directory; do
    [[ -d "$directory/src" ]] || continue

    is_unfinished "$directory" && continue

    package_manifest="$directory/package.yaml"
    if [[ ! -f "$package_manifest" ]]; then
        echo "No package.yaml found for Stack package: $directory" >&2
        exit 1
    fi

    package=$(sed -n 's/^name:[[:space:]]*//p' "$package_manifest" | head -n 1)
    if [[ -z "$package" ]]; then
        echo "No package name found in $package_manifest" >&2
        exit 1
    fi
    packages+=("$package")
done < <(awk '
    $0 == "packages:" { in_packages = 1; next }
    in_packages && /^[^[:space:]-]/ { exit }
    in_packages && /^- / { print substr($0, 3) }
' stack.yaml)

if [[ ${#packages[@]} -eq 0 ]]; then
    echo "No implemented Stack packages found" >&2
    exit 1
fi

if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
    printf 'packages=%s\n' "${packages[*]}" >> "$GITHUB_OUTPUT"
    paths_json=$(printf '"%s",' "${hlint_paths[@]}")
    printf 'hlint_paths=[%s]\n' "${paths_json%,}" >> "$GITHUB_OUTPUT"
else
    printf 'packages=%s\n' "${packages[*]}"
    printf 'hlint_paths=%s\n' "$(printf '%s ' "${hlint_paths[@]}")"
fi
