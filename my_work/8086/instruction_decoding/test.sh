current_dir="$(cd "$(dirname "${BASH_SOURCE:-$0}")" && pwd)"

mkdir -p "$current_dir/temp"

set -e

cc -Wall -Wextra -o "$current_dir/dasm8086" "$current_dir/dasm8086.c"

test_listing() {
    local name=$1
    if [[ -z "$name" ]]; then
        echo "Usage: test <name>"
        echo "<name> not provided!"
        exit 69
    fi

    # create decoded asm file from ground truth
    "$current_dir/dasm8086" "$current_dir/ground_truth/$name" > "$current_dir/temp/$name.decoded.asm"

    # compare ground truth and decoded asm file
    nasm "$current_dir/temp/$name.decoded.asm"
    diff "$current_dir/temp/$name.decoded" "$current_dir/ground_truth/$name"
    echo "$name Success!"
}

test_listing "listing_0037_single_register_mov"
test_listing "listing_0038_many_register_mov"
test_listing "listing_0039_more_movs"
test_listing "listing_0040_challenge_movs"

set +e
