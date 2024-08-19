#!/usr/bin/env bash

# https://askubuntu.com/questions/160945/is-there-a-way-to-disable-a-laptops-internal-keyboard

# TODO: error check keyboard name/id
# FIXME type is no longer shown when disabled, and device is "floating". figure out how to handle properly.

function ids() {
    local name=$1 && shift
    local type=$2 && shift
    xinput list | perl -lne "/(?:∼ |↳ )${name}\s+id=([0-9]+)\s+\[(?:floating )?(?:master|slave)(?:\s+${type}\s)?/ && print \$1"
}

function mouse_ids() {
    local name=$1 && shift
    ids "${name}" "pointer"
}

function keyboard_ids() {
    local name=$1 && shift
    ids "${name}" "keyboard"
}

function keyboard_enabled() {
    local id=$1 && shift
    xinput list-props "${id}" | awk -F: '/Device Enabled/ { exit $2 != 1 }'
}

function enable_keyboard() {
    local id=$1 && shift

    local icon_path=~/Documents/keyboard-enable.png
    notify-send -i "${icon_path}" "Enabling keyboard..." "keyboard id ${id}"
    echo "enabling keyboard"
    xinput enable "${id}"
    if keyboard_enabled "${id}"; then
        notify-send --icon "${icon_path}" "Enabling keyboard..." "ON - keyboard enabled"
        echo "enabled keyboard"
    else
        notify-send --icon "${icon_path}" "Enabling keyboard..." "FAILED - keyboard still DISABLED"
        echo "failed to enable keyboard"
    fi
}

function disable_keyboard() {
    local id=$1 && shift

    local icon_path=~/Documents/keyboard-disable.png
    notify-send -i "${icon_path}" "Disabling keyboard..." "keyboard id ${id}"
    echo "disabling keyboard"
    xinput disable "${id}"
    if keyboard_enabled "${id}"; then
        notify-send -i "${icon_path}" "Disabling keyboard..." "FAILED - keyboard still enabled"
        echo "failed to disable keyboard"
    else
        notify-send -i "${icon_path}" "Disabling keyboard..." "OFF - keyboard disabled"
        echo "disabled keyboard"
    fi
}

function toggle_keyboard() {
    local id=$1 && shift

    if keyboard_enabled "${id}"; then
        disable_keyboard "${id}"
    else
        enable_keyboard "${id}"
    fi
}

function do_toggling() {
    local id
    for id in $(keyboard_ids "Corsair Corsair K70 RGB Gaming Keyboard"); do
        toggle_keyboard "${id}"
    done
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    do_toggling
fi
